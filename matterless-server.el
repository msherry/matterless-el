;;; matterless-server.el ---  -*- lexical-binding: t; -*-

;;; Copyright (C) 2018 Gaelan D'costa

;;; Author: Gaelan D'costa <gdcosta@gmail.com>
;;; Created: December 08, 2018
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

;; Mattermost -- unlike Slack -- can be self-hosted.  Thus, we need to be able
;; to register multiple servers since we might talk to multiple MM instances
;; and this is the layer where establishing connections and auth will happen.

;;; Code:
(require 'cl-lib)
(require 'eieio)
(require 'matterless-util)

(defvar matterless-servers
  "List of MM teams we sign into"
  nil)

(defclass matterless-server ()
  ((uri :initarg :uri)
   (token :initarg :token :initform nil)
   (client-id :initarg :client-id)
   (client-secret :initarg :client-secret)))

(cl-defmethod matterless-equalp ((this matterless-server) other)
  (string= (oref this uri) (oref other uri)))

(defun matterless-create-server (plist)
  "Create MM server object from PLIST."
  (let ((server (apply #'make-instance 'matterless-server
                     (matterless-collect-slots 'matterless-server plist))))
    server))

;;;###autoload
(defun matterless-register-server (&rest plist)
  "PLIST must contain values for :server-uri :client-id :client-secret.
Setting :token with session or personal one will reduce configuration steps."
  (interactive
   (let ((server-uri (read-from-minibuffer "Server URL: "))
         (client-id (read-from-minibuffer "Login ID: "))
         (client-secret (read-from-minibuffer "Login Secret/Password: "))
         (token nil))
     (unless (and client-id (< 0 (length client-id))
                  client-secret (< 0 (length client-secret)))
       (setq token (read-from-minibuffer "Session/Personal Token: ")))
     (list :server-uri server-uri :client-id client-id
           :client-secret client-secret :token token)))
  (cl-labels ((has-client-id-and-client-secret-p
               (plist)
               (let ((id (plist-get plist :client-id))
                     (secret (plist-get plist :client-secret)))
                 (and id secret (< 0 (length id)) (< 0 (length secret)))))
              (has-token-p (plist)
                           (let ((token (plist-get plist :token)))
                             (and token (< 0 (length token)))))
              (register (server)
                        (setq matterless-servers
                              (cons server
                                    (cl-remove-if
                                     #'(lambda (other)
                                         (matterless-equalp server
                                                            other))
                                     matterless-servers)))))
    (if (or (has-client-id-and-client-secret-p plist)
            (has-token-p plist))
        (let ((server (matterless-create-server plist)))
          (register server))
      (error ":client-id and :client-secret or :token is required"))))

(provide 'matterless-server)
;;; matterless-server.el ends here
