;;; matterless.el --- MatterMost client for Emacs -*- lexical-binding: t; -*-

;;; Copyright (C) 2018 Gaelan D'costa

;;; Author: Gaelan D'costa <gdcosta@gmail.com>
;;; Created: October 21, 2018
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
(require 'matterless-message-notification)

;;;###autoload
(defun matterless-start ()
  "Start the Mattermost chat client."
  (interactive)
  (matterless-enable-modeline))

(provide 'matterless)
;;; matterless.el ends here
