;;; lispy-autoreconnect.el --- autoreconnect feature for lispy

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords: comm

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; autoreconnect facility. This feature was created for ErBot, you'd better
;; think twice before using it. If you really want to, here is a sample:
;;
;; (require 'lispy-auto-reconnect)
;; (setq lispy-reconnect-ping (lambda () (lispy-message "plop")))
;; (setq lispy-timer-delay 300)
;;
;; This will launch the message every 5 minutes if nothing has been received
;; during the last interval. If no message is received in response, the
;; reconnection is activated

;;; Code:

(require 'lispy-utils)
(require 'lispy-vars)

(lispy-defvar lispy-inhibit-reconnect nil "")
(lispy-defvar lispy-reconnect-status nil "")
(defvar lispy-reconnect-ping nil)
(lispy-defvar lispy-timer nil "")
(defvar lispy-timer-delay nil "")

(defun lispy-reconnect ()
  (interactive)
  (unless lispy-inhibit-reconnect
    (progn
      (let ((lispy-inhibit-reconnect t))
        (run-hooks 'lispy-disconnected-hook))
      (lispy lispy-host lispy-port lispy-buffer)
      (lispy-update-buffer-hierarchy (current-buffer) 'lispy-process)
      (goto-char (point-max)))))

(defadvice lispy-quit (before before-lispy-quit act)
  (lispy-stop-timer)
  (setq lispy-inhibit-reconnect t)
  (lispy-update-buffer-hierarchy (current-buffer) 'lispy-inhibit-reconnect))

(defun lispy-check-reconnect ()
  (cond ((equal lispy-reconnect-status nil)
         (setq lispy-reconnect-status 'ask))
        ((equal lispy-reconnect-status 'ask)
         (when lispy-reconnect-ping
           (funcall lispy-reconnect-ping)
           (setq lispy-reconnect-status 'reconnect)))
        ((equal lispy-reconnect-status 'reconnect)
         (lispy-reconnect))))

(defun lispy-start-timer ()
  (setq lispy-reconnect-status nil)
  (when lispy-timer-delay
    (setq lispy-timer (run-with-timer lispy-timer-delay lispy-timer-delay 'lispy-check-reconnect))))

(defun lispy-stop-timer ()
  (when lispy-timer
    (cancel-timer lispy-timer)
    (setq lispy-timer nil)))

(defun lispy-reset-timer (&rest args)
  (lispy-stop-timer)
  (lispy-start-timer))

(define-key lispy-mode-map "\C-cR" 'lispy-reconnect)

(add-hook 'lispy-pre-insert-hook (lambda (&rest args)
                                   (setq lispy-reconnect-status nil)))
(add-hook 'lispy-disconnected-hook 'lispy-reconnect)
(add-hook 'lispy-connected-hook 'lispy-start-timer)

(provide 'lispy-autoreconnect)
;;; lispy-autoreconnect.el ends here
