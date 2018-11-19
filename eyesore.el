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
				   (pop sub)
				 (let ((value (eyesore-parse sub (cadr spec))))
				   (loop repeat
					 (1+
					  (length (or (cddr (assoc
							     (car value)
							     eyesore-commands))
						      '())))
					 do (pop sub))
				   value)))))))))))

(provide 'eyesore)

;;; eyesore.el ends here
