;;; lispy-utils.el --- utils functions for lispy

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

;;

;;; Code:

(require 'lispy-vars)

(defun lispy-case-unsensitive (s)
  "Generate a regexp for matching S case unsensitive."
  (let ((up (upcase s)) (down (downcase s)))
    (lispy-generate-regexp (string-to-list up) (string-to-list down)))
)

(defun lispy-generate-regexp (s1 s2)
  "Generate mixed regexp from S1 and S2."
  (and s1
       (concat (concat "[" (char-to-string (car s1)) (char-to-string (car s2)) "]")
	       (lispy-generate-regexp (cdr s1) (cdr s2))
	       )))

(defun lispy-region-or-word ()
  "If mark is active, return the selected region, else return word at point."
  (if mark-active
      (prog1
          (buffer-substring (region-beginning) (region-end))
        (keyboard-quit))
    (word-at-point)))

(defun lispy-turn-on-echo ()
  "Turn echo on."
;;  (message "echo turned on")
  (setq lispy-echo-off nil))

(defun lispy-turn-off-echo ()
  "Turn echo off."
;;  (message "echo turned off")
  (setq lispy-echo-off t))

(defun lispy-clear-text-area ()
  "Clear the typing region."
  (delete-region (point-min) (point-max)))

(defun lispy-beep ()
  (ding))

(defun lispy-message (s)
  "Send the message S to the lispy process"
  (interactive)
  (and s
       (progn
         (run-hook-with-args 'lispy-pre-message-hook s)
         (condition-case nil
             (progn
               (process-send-string lispy-process (concat (replace-regexp-in-string
                                                           (string 255)
                                                           (string 255 255) s)
                                                          "\r\n"))
               (run-hook-with-args 'lispy-post-message-hook s))
           (error (progn
;;                    (message "plop")
                    (lispy-sentinel lispy-process "disconnected"))))
         )))

(defun lispy-send-entry (prefix &optional default)
  (lispy-reach-sending-buffer)
  (insert (concat prefix " " default)))

(defun lispy-send ()
  "Send the text in current buffer."
  (interactive)
  (let ((s (buffer-string)))
    (if (not (equal s ""))
        (progn
          (lispy-clear-text-area)
          (run-hooks 'lispy-pre-send-hook)
          (lispy-message (concat lispy-send-prefix s))
          ))))

(defun lispy-read-password ()
  (or lispy-password (read-passwd "")))

(defun lispy-fetch-user-list (string)
  "Fetch user list from server, receiving STRING. To be used from `lispy-pre-insert-hook'."
  (if lispy-read-user-list
        (cond
         ((string-match "^<Mtp> There " string)
          (setq lispy-read-user-list nil
                lispy-insert-line 'next)
          (remove-hook 'lispy-pre-insert-hook 'lispy-fetch-user-list))
         ((string-match "^\\(\\w+\\) +" string)
          (add-to-list 'lispy-user-list (match-string 1 string) t)))
    (if (string-match "^ Login " string)
        (progn
          (setq lispy-read-user-list t
                lispy-insert-line nil)))))

(defun lispy-quit ()
  "Close connection softly by sending appropriate message to server."
  (interactive)
  (setq lispy-inhibit-sentinel t)
  (lispy-update-buffer-hierarchy (current-buffer) 'lispy-inhibit-sentinel)
  (lispy-message "quit"))

(defun lispy-split-string (str sep)
  (if (>= lispy-emacs-version 210350)
      (split-string str sep t)
    (split-string str sep)))

(defun lispy-add-hook-once (hook function &optional append local)
  (let ((code (list 'lambda)))
    (setcdr code `(() (,function) (remove-hook ',hook ',code ',local)))
    (add-hook hook code append local)))

(lispy-defvar lispy-completion-tmp nil)

(require 'thingatpt)
(defun lispy-complete-nickname ()
  (interactive)
  (let ((wap (word-at-point))
        (current-completion nil)
        (min-completion nil))
    (if wap
        (progn
          (if (eq last-command this-command)
              (setq min-completion wap)
            (setq lispy-completion-tmp wap))
          (dolist (nick lispy-user-list)
            (when (and (string-match (format "^%s" lispy-completion-tmp) nick)
                       ;;(string< lispy-completion-tmp nick)
                       (or (not current-completion)
                           (string< nick current-completion))
                       (or (not min-completion)
                           (string< min-completion nick)))
              (setq current-completion nick)))
          (when current-completion
            (progn
              (backward-delete-char (length wap))
              (insert current-completion)))))))

(provide 'lispy-utils)
;;; lispy-utils.el ends here
