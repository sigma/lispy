;;; lispy-vars.el ---

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

(defvar lispy-buffer-local-variables nil)
(defvar lispy-buffer-local-hierarchy nil)

(defmacro lispy-defvar (symbol &optional initvalue docstring)
  (declare (indent 2))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (add-to-list 'lispy-buffer-local-variables ',symbol)))

(defun lispy-inherit-buffer-local-variables (from &optional to)
  (let* ((orig from)
         (target (or to (current-buffer)))
         (values (mapcar (lambda (s) (cons s (symbol-value-in-buffer s orig))) lispy-buffer-local-variables))
         (node (assoc orig lispy-buffer-local-hierarchy)))
    (if node
        (unless (member target (cdr node))
          (setcdr node (append (cdr node) (list target))))
      (add-to-list 'lispy-buffer-local-hierarchy (cons orig (list target))))
    (save-excursion
      (set-buffer target)
      (mapc (lambda (v) (set (car v) (cdr v))) values))))

(defun lispy-clean-buffer-hierarchy (buffer)
  (setq lispy-buffer-local-hierarchy
        (remove nil
                (mapcar (lambda (l)
                          (if (equal (car l) buffer) nil
                            (remove buffer l)))
                        lispy-buffer-local-hierarchy))))

(defun lispy-update-buffer-hierarchy (buffer &rest variables)
  (mapc (lambda (variable)
          (let ((value (symbol-value-in-buffer variable buffer))
                (node (assoc buffer lispy-buffer-local-hierarchy)))
            (when node
              (mapc (lambda (b)
                      (if (buffer-live-p b)
                          (save-excursion
                            (set-buffer b)
                            (set variable value)
                            (lispy-update-buffer-hierarchy b variable))
                        (lispy-clean-buffer-hierarchy b)))
                    (cdr node)))))
        variables))

(defvar lispy-version "Lispy 0.5"
  "*Version of the program")

(defvar lispy-mode-hook nil "*Hook to run after setting current buffer to lispy-mode.")
(defvar lispy-pre-insert-hook nil "Hook to run before inserting a new line. Functions are called with the string as argument")
(defvar lispy-post-insert-hook nil "Hook to run after inserting a new line. Functions are called with the string as argument")
(defvar lispy-pre-send-hook nil "")
(defvar lispy-pre-message-hook nil "")
(defvar lispy-post-message-hook nil "")
(defvar lispy-connected-hook nil "")
(defvar lispy-disconnected-hook nil "")
(defvar lispy-exit-hook nil "")

(defvar lispy-default-host "localhost")
(defvar lispy-default-port "5000")

(lispy-defvar lispy-host nil)
(lispy-defvar lispy-port nil)
(lispy-defvar lispy-remote-user nil "")
(lispy-defvar lispy-password nil "")
(lispy-defvar lispy-echo-off nil "")
(lispy-defvar lispy-read-user-list nil "")
(lispy-defvar lispy-user-list nil "")
(lispy-defvar lispy-insert-line t "")
(lispy-defvar lispy-require-end-of-line nil "")
(lispy-defvar lispy-insert-buffer nil "")
(lispy-defvar lispy-connected nil "")

(lispy-defvar lispy-inhibit-sentinel nil "")
(lispy-defvar lispy-inhibit-reconnect nil "")

(defvar lispy-mode-map '())
(defvar lispy-send-mode-map '())

(lispy-defvar lispy-process nil)

(defvar lispy-buffer nil)
(defvar lispy-buffer-name "*<Mtp> Chat*")

(defvar lispy-send-buffer nil)
(defvar lispy-send-buffer-name "*<Mtp> Chat* -- send")
(defvar lispy-send-buffer-height 5)
(lispy-defvar lispy-send-prefix "")

(defvar lispy-telnet-sequences '(
                                 ("ÿû" . lispy-turn-off-echo)
                                 ("ÿü" . lispy-turn-on-echo)
                                 ("" . lispy-beep)
                                 ("[2J[0;0H" . lispy-clear-text-area)) "")

(provide 'lispy-vars)
;;; lispy-vars.el ends here
