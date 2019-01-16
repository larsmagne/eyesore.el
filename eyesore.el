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
    (alias nil :alias)
    (unique nil :unique)
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
    (arranger nil :name)
    (coarranger nil :name)
    (excerpt nil)
    (remix nil)
    (live nil)
    (coremixer nil :name)
    (remixer nil :name)
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
  (insert (format "<div class='eyesore'><h1><a href='https://eyesore.no/html/year/%s.html'>%s</a></h1>\n<table>\n"
		  year
		  year))
  (loop for elem in (getf eyesore-data :years)
	when (equal year (getf elem :year))
	do (eyesore-format-releases (getf elem :releases)))
  (insert "</table></div>\nThis post is part of the <a href='https://lars.ingebrigtsen.no/2018/11/19/4ad-1980/'>chronological look at all 4AD releases</a>, year by year.\n\n*) Missing from Spotify.\n"))

(defun eyesore-best-format (release)
  (loop with candidate
	for elem in (getf release :details)
	for formats = (if (stringp (getf elem :formats))
			  (list (getf elem :formats))
			(loop for format in (getf elem :formats)
			      collect (getf format :name)))
	when (and (eq (getf elem :type) 'formats)
		  (or (null candidate)
		      (member "BAD" formats)
		      (member "BAD CD" formats)))
	do (setq candidate elem)
	finally (return candidate)))

(defun eyesore-format-releases (releases &optional best-only)
  (dolist (release releases)
    (let ((id (getf release :id)))
      (when (and id
		 (not (string-match "^NON" id)))
	(dolist (format
		 (if best-only
		     (list (eyesore-best-format release))
		   (loop for elem in (getf release :details)
			 when (eq (getf elem :type) 'formats)
			 collect elem)))
	  (let ((image (or (eyesore-sleeve-image
			    id (getf release :group)
			    (getf release :album)
			    (eyesore-format-names format))
			   (eyesore-external release))))
	    (insert
	     (format
	      "<tr><td>%s</td><td>%s&nbsp;%s<br><a href='%s'>%s</a> -- %s<p>%s</td></tr>\n\n"
	      (if (not image)
		  ""
		(format "<a href='https://eyesore.no/html/wrap/%s.html'><img src='https://eyesore.no/html/gif/%s'></a>"
			(replace-regexp-in-string "[.]gif$" ".jpg" image)
			image))
	      (eyesore-format-imgs release)
	      (eyesore-spec id (car (eyesore-formats format)))
	      (eyesore-group-link (getf release :group))
	      (eyesore-string (getf release :group))
	      (eyesore-string (getf release :album))
	      (mapconcat #'identity (eyesore-tracks format) ", ")))))))))

(defun eyesore-tracks (format)
  (loop for elem in (getf format :tracks)
	append (cond
		((eq (getf elem :type) 'track)
		 (let ((name (getf elem :name))
		       nn)
		   (loop for d in (getf elem :details)
			 when (and (getf d :name)
				   (null (getf d :type)))
			 return (setq nn 
				      (format "%s (%s)" name (getf d :name)))
			 do (cond
			     ((eq (getf d :type) 'group)
			      (setq name (format "%s / %s" (getf d :name)
						 name)))
			     ((eq (getf d :type) 'remix)
			      (setq nn (format "%s (remix)" name)))
			     ((eq (getf d :type) 'rerecorded)
			      (setq nn (format "%s (rerecorded)" name)))
			     ((eq (getf d :type) 'live)
			      (setq nn (format "%s (live)" name)))))
		   (list (or nn name))))
		((eq (getf elem :type) 'includes)
		 (eyesore-find-tracks (getf elem :id)
				      (getf elem :format))))))

(defun eyesore-formats (format)
  (cond
   ((stringp (getf format :formats))
    (list (getf format :formats)))
   ((stringp (car (getf format :formats)))
    (getf format :formats))
   (t
    (loop for f in (getf format :formats)
	  collect (getf f :name)))))

(defun eyesore-find-tracks (id format)
  (loop for elem in (getf eyesore-data :years)
	append
	(loop for release in (getf elem :releases)
	      when (equal (getf release :id) id)
	      append
	      (loop for elem in (getf release :details)
		    when (and (eq (getf elem :type) 'formats)
			      (member format (eyesore-formats elem)))
		    return (eyesore-tracks elem)))))

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
	;; Find a good match.
	(loop for image in images
	      for bits = (split-string image "[.]")
	      when (and (equalp (eyesore-normalise group)
				(eyesore-imgize (nth 0 bits)))
			(equalp (eyesore-normalise album)
				(eyesore-imgize (nth 1 bits)))
			(eyesore-format-equal formats
					      (eyesore-imgize (nth 2 bits))))
	      return image)
	;; Just match group/album.
	(loop for image in images
	      for bits = (split-string image "[.]")
	      when (and (equalp (eyesore-normalise group)
				(eyesore-imgize (nth 0 bits)))
			(equalp (eyesore-normalise album)
				(eyesore-imgize (nth 1 bits))))
	      return image))))

(defun eyesore-format-equal (formats bit)
  (loop for format in formats
	when (equal (cond
		     ((string-match "^AD" format) "7")
		     ((string-match "^CAD CD" format) "cd")
		     ((string-match "^CAD C" format) "cas")
		     ((string-match "^CAD" format) "lp")
		     ((string-match "^BAD CD" format) "cdsingle")
		     ((string-match "^BAD C" format) "cassingle")
		     ((string-match "^BAD" format) "ep")
		     ((string-match "^AD C" format) "cassingle"))
		    bit)
	return t))


(defun eyesore-imgize (string)
  (eyesore-normalise
   (with-temp-buffer
     (insert string)
     (goto-char (point-min))
     (forward-char 1)
     (let ((case-fold-search nil))
       (while (re-search-forward "[A-Z]" nil t)
	 (goto-char (match-beginning 0))
	 (insert " ")
	 (forward-char 1)))
     (goto-char (point-min))
     (while (re-search-forward "[0-9]+" nil t)
       (insert " ")
       (save-excursion
	 (goto-char (match-beginning 0))
	 (insert " ")))
     (buffer-string))))

(defun eyesore-group-link (group)
  (format "https://eyesore.no/html/group/%s.html" (eyesore-normalise group)))

(defun eyesore-number (number)
  (let* ((low '("zero" "one" "two" "three" "four" "five" "six"
		"seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen"
		"fourteen" "fifteen" "sixteen" "seventeen" "eighteen"
		"nineteen"))
	 (tens '(nil nil "twenty" "thirty" "fourty" "fifty" "sixty" "seventy"
		     "eighty" "ninety"))
	 (high '((1000 "thousand")
		 (1000000 "million")
		 (1000000000 "billion"))))
    (cond
     ((< number 20)
      (elt low number))
     ((< number 100)
      (format "%s %s"
	      (elt tens (/ number 10))
	      (elt low (mod number 10))))
     ((< number 1000)
      (format "%s hundred %s"
	      (elt low (/ number 100))
	      (eyesore-number (mod number 100))))
     (t
      (loop for (num name) in high
	    when (< num number (* num 1000))
	    return (format "%s %s %s"
			   (eyesore-number (/ number num))
			   name
			   (eyesore-number (mod number num))))))))

(defun eyesore-normalise (string)
  (if (string-match "|[*=]" string)
      (substring string (match-end 0))
    (setq string (replace-regexp-in-string "|.*" "" string))
    (setq string (replace-regexp-in-string "^the " "" string))
    (setq string (replace-regexp-in-string "^a " "" string))
    (setq string (replace-regexp-in-string "^an " "" string))
    (setq string (replace-regexp-in-string "&" "and" string))
    (setq string
	  (mapconcat
	   (lambda (word)
	     (if (string-match "^[0-9]+$" word)
		 (eyesore-number (string-to-number word))
	       word))
	   (split-string string " " t)
	   ""))
    (setq string (replace-regexp-in-string " " "" string))
    (setq string (replace-regexp-in-string "[^a-z]" "" string))
    (downcase string)))

(defun eyesore-release-format-names (release)
  (loop for elem in (getf release :details)
	when (eq (getf elem :type) 'formats)
	append (if (stringp (getf elem :formats))
		   (list (getf elem :formats))
		 (loop for format in (getf elem :formats)
		       collect (getf format :name)))))

(defun eyesore-format-names (format)
  (if (stringp (getf format :formats))
      (list (getf format :formats))
    (loop for format in (getf format :formats)
	  collect (getf format :name))))

(defun eyesore-format-imgs (release)
  (mapconcat
   'identity
   (loop for format in (eyesore-release-format-names release)
	 for gif = (cond
		    ((string-match "7\"" format) "7")
		    ((string-match "^AD" format) "7")
		    ((string-match "^CAD CD" format) "cd")
		    ((string-match "^CAD C" format) "cas")
		    ((string-match "^CAD" format) "lp")
		    ((string-match "^BAD CD" format) "cd5")
		    ((string-match "^BAD C" format) "cassingle")
		    ((string-match "^BAD" format) "12")
		    ((string-match "^AD C" format) "cassingle"))
	 when gif
	 collect (format "<img src='https://eyesore.no/html/bullet/%s.gif'>" gif))
   ""))

(defun eyesore-string (string)
  (replace-regexp-in-string "|.*" "" string))

(provide 'eyesore)

;;; eyesore.el ends here
