;;; eyesore.el --- Parse .eye files -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; eyesore is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; eyesore is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:

;;; Code:

(require 'cl)

(defvar eyesore-commands
  '((ryear* nil :year :releases)
    (discography nil :years)
    (year nil :year)
    (release formats :id :group :album :details)
    (formats track :formats :tracks)
    (includes nil :id :format)
    (date nil :date)
    (track nil :name :details)
    (writer nil :name)
    (coproducer nil :name)
    (producer nil :name)
    (groove nil :name)
    (comments nil :name)
    (external nil :name)
    (cowriter nil :name)
    (band nil :name)
    (enter nil :name)
    (exit nil :name)
    (time nil :name)
    (featured nil :name)
    (studio nil :name)
    (sleeve nil :name)
    (excerpt nil)
    (remix nil)
    (live nil)
    (engineer nil :name)
    (group nil :name)
    (ephemera nil :name)))

(defun eyesore-read ()
  (eyesore-skip-whitespace)
  (let ((char (following-char)))
    (cond
     ((eql char ?\{)
      (forward-char 1)
      (eyesore-read-string))
     ((eql char ?\[)
      (forward-char 1)
      (eyesore-read-list))
     ((eql char ?\~)
      (forward-char 1)
      :default)
     (t
      (eyesore-read-word)))))

(defun eyesore-read-string ()
  (coerce
   (loop for char = (prog1
			(following-char)
		      (forward-char 1))
	 while (not (eql char ?\}))
	 collect char)
   'string))

(defun eyesore-read-word ()
  (intern
   (coerce
    (loop for char = (following-char)
	  while (or (<= ?a char ?z)
		    (<= ?A char ?A)
		    (<= ?0 char ?9)
		    (eql char ?\+)
		    (eql char ?\*))
	  collect char
	  do (forward-char 1))
    'string)
   obarray))

(defun eyesore-read-list ()
  (loop while (and
	       (progn
		 (eyesore-skip-whitespace)
		 (not (eql (following-char) ?\])))
	       (not (eobp)))
	collect (eyesore-read)
	finally (unless (eobp)
		  (forward-char 1))))

(defun eyesore-skip-whitespace ()
  (while
      (cond
       ((looking-at "[ \t\n]+")
	(goto-char (match-end 0)))
       ((looking-at "/\\*")
	(goto-char (match-end 0))
	(loop with level = 1
	      while (> level 0)
	      do (re-search-forward "\\*/\\|/\\*")
	      (if (equal (match-string 0) "/*")
		  (incf level)
		(decf level)))
	t)
       (t nil))
    ))

(defun eyesore-parse (elem &optional default)
  (let ((type (pop elem)))
    (when (eq type :default)
      (setq type default))
    (let ((spec (assoc type eyesore-commands)))
      (append
       (list :type type)
       (loop for field in (cddr spec)
	     append (list
		     field
		     (let ((sub (pop elem)))
		       (if (stringp sub)
			   sub
			 (loop while sub
			       collect
			       (if (stringp (car sub))
				   (list :type (cadr spec)
					 :name (pop sub))
				 (let ((value (eyesore-parse sub (cadr spec))))
				   (loop repeat
					 (1+
					  (length (or (cddr (assoc
							     (cadr value)
							     eyesore-commands))
						      '())))
					 do (pop sub))
				   value)))))))))))

(defun eyesore-parse-file ()
  (setq eyesore-data
	(with-temp-buffer
	  (insert-file-contents "~/Catalogue/data/4ad.eye")
	  (eyesore-parse
	   (loop while (not (eobp))
		 collect (eyesore-read))))))

(defun eyesore-format-year (year)
  (insert (format "<div class='eyesore'><h1>%s</h1>\n<table>\n" year))
  (loop for elem in (getf eyesore-data :years)
	when (equal year (getf elem :year))
	do (eyesore-format-releases (getf elem :releases)))
  (insert "</table></div>"))

(defun eyesore-format-releases (releases)
  (dolist (release releases)
    (let ((id (getf release :id)))
      (unless (string-match "^NON" id)
	(let ((image (or (eyesore-sleeve-image
			  id (getf release :group)
			  (getf release :album)
			  (eyesore-formats release))
			 (eyesore-external release))))
	  (insert
	   (format
	    "<tr><td><a href='%s'><img src='%s'></a><td>%s&nbsp;%s<br><a href='%s'>%s</a> -- %s<p>%s\n\n"
	    (and image
		 (format "https://eyesore.no/html/wrap/%s.html"
			 (replace-regexp-in-string "[.]gif$" ".jpg" image)))
	    (and image
		 (format "https://eyesore.no/html/gif/%s" image))
	    (eyesore-format-imgs release)
	    (eyesore-spec id (car (eyesore-formats release)))
	    (eyesore-group-link (getf release :group))
	    (eyesore-string (getf release :group))
	    (eyesore-string (getf release :album))
	    (mapconcat
	     #'identity
	     (loop for track in (getf (car (getf release :details))
				      :tracks)
		   when (eq (getf track :type) 'track)
		   collect (getf track :name))
	     ", "))))))))

(defun eyesore-external (release)
  (loop for track in (getf (car (getf release :details))
				      :tracks)
	when (and (eq (getf track :type) 'external)
		  (string-match "[.]jpg$"(getf track :name)))
	return (replace-regexp-in-string "[.]jpg$" ".gif"
					 (getf track :name))))

(defun eyesore-spec (id format)
  (cond
   ((string-match "AXIS" id)
    id)
   ((equal "CAD.*CD" format)
    (format (replace-regexp-in-string " " "%s" format) id))
   ((equal "BAD.*CD" format)
    (format (replace-regexp-in-string " " "%s" format) id))
   ((equal "CAD" format)
    (format "CAD%s" id))
   ((not (string-match " " format))
    (concat format id))
   (t
    (concat format id))))

(defun eyesore-sleeve-image (id group album formats)
  (let* ((images (directory-files "~/Catalogue/html/gif/" nil "[.]gif$"))
	 (direct
	  (loop for format in formats
		for spec = (eyesore-spec id format)
		return (loop for image in images
			     for bits = (split-string image "[.]")
			     when (equal (nth 2 bits) spec)
			     return image))))
    (or direct
	(loop for image in images
	      for bits = (split-string image "[.]")
	      when (and (equalp (eyesore-imgize group)
				(eyesore-imgize (nth 0 bits)))
			(equalp (eyesore-imgize album)
				(eyesore-imgize (nth 1 bits))))
	      return image))))

(defun eyesore-imgize (string)
  (replace-regexp-in-string "[^A-Za-z0-9]" "" string))

(defun eyesore-group-link (group)
  (format "https://eyesore.no/html/group/%s.html" (eyesore-normalise group)))

(defun eyesore-normalise (string)
  (setq string (replace-regexp-in-string "^the " "" string))
  (setq string (replace-regexp-in-string "^a " "" string))
  (setq string (replace-regexp-in-string "^an " "" string))
  (setq string (replace-regexp-in-string " " "" string))
  (downcase string))

(defun eyesore-formats (release)
  (loop for elem in (getf release :details)
	when (eq (getf elem :type) 'formats)
	append (if (stringp (getf elem :formats))
		   (list (getf elem :formats))
		 (loop for format in (getf elem :formats)
		       collect (getf format :name)))))

(defun eyesore-format-imgs (release)
  (mapconcat
   'identity
   (loop for format in (eyesore-formats release)
	 for gif = (cond
		    ((string-match "7\"" format) "7")
		    ((string-match "^AD" format) "7")
		    ((string-match "^CAD C" format) "cas")
		    ((string-match "^CAD CD" format) "cd")
		    ((string-match "^CAD" format) "lp")
		    ((string-match "^BAD CD" format) "cd5")
		    ((string-match "^BAD C" format) "cassingled")
		    ((string-match "^BAD" format) "12")
		    ((string-match "^AD C" format) "cassingle"))
	 when gif
	 collect (format "<img src='https://eyesore.no/html/bullet/%s.gif'>" gif))
   ""))

(defun eyesore-string (string)
  (replace-regexp-in-string "|.*" "" string))

(provide 'eyesore)

;;; eyesore.el ends here
