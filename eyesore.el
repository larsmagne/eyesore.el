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
  '((year 1)
    (release 3 formats)
    (formats 1 track)
    (date 1)
    (track 1)
    (writer 1)
    (coproducer 1)
    (producer 1)
    (groove 1)
    (comments 1)
    (external 1)
    (cowriter 1)
    (band 1)
    (enter 1)
    (exit 1)
    (time 1)
    (featured 1)
    (studio 1)
    (sleeve 1)
    (excerpt 0)
    (remix 0)
    (live 0)
    (engineer 1)
    (group 1)
    (ephemera 1)))

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
  (coerce
   (loop for char = (following-char)
	 while (or (<= ?a char ?z)
		   (<= ?A char ?A)
		   (<= ?0 char ?9)
		   (eql char ?\+)
		   (eql char ?\*))
	 collect char
	 do (forward-char 1))
   'string))

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

(provide 'eyesore)

;;; eyesore.el ends here
