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
  (setq lispy-read-password nil))

(defun lispy-turn-off-echo ()
  "Turn echo off."
  (setq lispy-read-password t))

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
         (run-hook-with-args 'lispy-pre-send-hook s)
         (process-send-string lispy-process (concat s "\r\n"))
         (run-hook-with-args 'lispy-post-send-hook s)
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
          (lispy-message (concat lispy-send-prefix s))
          ))))

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
  (lispy-message "quit"))

(defconst lispy-emacs-version
  (progn
    (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
    (+ (* (string-to-int (match-string 1 emacs-version)) 10000)
     (* (string-to-int (match-string 2 emacs-version)) 100)
     (string-to-int (match-string 3 emacs-version)))))

(defun lispy-split-string (str sep)
  (if (>= lispy-emacs-version 210350)
      (split-string str sep t)
    (split-string str sep)))

(provide 'lispy-utils)
;;; lispy-utils.el ends here
