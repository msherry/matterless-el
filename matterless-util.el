;;; matterless-util.el --- Matterless helper utils -*- lexical-binding: t; -*-

;;; Copyright (C) 2018 Gaelan D'costa

;;; Author: Gaelan D'costa <gdcosta@gmail.com>
;;; Created: December 8, 2018
;;; Keywords: chat mattermost
;;; Homepage: https://github.com/RobotDisco/matterless-el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; I have no idea how Emacs Lisp programs are written, let's find out!

;; No doubt I will be shamelessly inspired by the amazing slack client written
;; by Yuya Minami over at https://github.com/yuya373/emacs-slack/
;; If I am very lucky the code structure will be very similar as I need someone
;; to guide me through the wasteland of my own beginnerhood.

;;; Code:
(defun matterless-seq-to-list (seq)
  "Convert SEQ into a proper list if necessary."
  (if (listp seq) seq (append seq nil)))

(defun matterless-class-have-slot-p (class slot)
  "Does CLASS have SLOT defined?"
  (and (symbolp slot)
	   (let* ((stripped (substring (symbol-name slot) 1))
			  (replaced (replace-regexp-in-string "_" "-"
												  stripped))
			  (symbolized (intern replaced)))
		 (slot-exists-p class symbolized))))

(defun matterless-collect-slots (class seq)
  "Transform SEQ into slot inputs appropriate for CLASS.
CLASS is the symbol of the class being instantiated.
SEQ is a sequence of slot values."
  (let ((plist (matterless-seq-to-list seq)))
	(cl-loop for p in plist
			 if (and (matterless-class-have-slot-p class p)
					 (plist-member plist p))
			 nconc (let ((value (plist-get plist p)))
					 (list p (if (stringp value)
								 (decode-coding-string value 'utf-8)
							   (if (eq :json-false value)
								   nil
								 value)))))))

(provide 'matterless-util)
;;; matterless-util.el ends here
