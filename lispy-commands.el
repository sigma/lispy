;;; lispy-commands.el --- Some basic commands for Lispy

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Yann Hodique <Yann.Hodique@lifl.fr>
;; Keywords:

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

;; This module provides basic commands and shortcuts for a chat session.
;; In order to use it, just
;;
;; (require 'lispy-commands)

;;; Code:

(require 'lispy-vars)
(require 'lispy-utils)

(defun lispy-away ()
  (interactive)
  (lispy-message "switch away\n"))

(defun lispy-tell ()
  (interactive)
  (lispy-send-entry "tell"))

(defun lispy-reply ()
  (interactive)
  (lispy-send-entry "reply"))

(defun lispy-emote ()
  (interactive)
  (lispy-send-entry "emote"))

(defun lispy-who ()
  (interactive)
  (lispy-message "who all"))

(defun lispy-finger ()
  (interactive)
  (lispy-message (concat "finger " (lispy-region-or-word))))

(defun lispy-kick ()
  (interactive)
  (lispy-send-entry (concat "kick " (lispy-region-or-word) " ")))

(defun lispy-url ()
  (interactive)
  (if mark-active
      (prog1
          (browse-url (buffer-substring (region-beginning) (region-end)))
        (keyboard-quit))
    (browse-url-at-point)))

(defun lispy-fetch ()
  (interactive)
  (add-hook 'lispy-pre-insert-hook 'lispy-fetch-user-list)
  (lispy-who))

(define-key lispy-mode-map "\C-ca" 'lispy-away)
(define-key lispy-mode-map "\C-ct" 'lispy-tell)
(define-key lispy-mode-map "\C-cr" 'lispy-reply)
(define-key lispy-mode-map "\C-ce" 'lispy-emote)
(define-key lispy-mode-map "\C-cw" 'lispy-who)
(define-key lispy-mode-map "\C-cf" 'lispy-finger)
(define-key lispy-mode-map "\C-ck" 'lispy-kick)
(define-key lispy-mode-map "\C-cq" 'lispy-quit)
(define-key lispy-mode-map "\C-cu" 'lispy-url)
(define-key lispy-mode-map "\C-c=" 'lispy-fetch)

(provide 'lispy-commands)
;;; lispy-commands.el ends here
