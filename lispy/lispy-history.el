;;; lispy-history.el --- provide history functions for lispy

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

(defvar lispy-history-commands nil "*History ring for lispy input arguments.")
(defvar lispy-history-max-size 100 "")
(defvar lispy-history-cursor -1 "")
(defvar lispy-current-line "")

(defun lispy-history-add-entry (str)
  (cond
   ((< (length lispy-history-commands) lispy-history-max-size)
    (setq lispy-history-commands (append (list str) lispy-history-commands)))
   (t
    (setq lispy-history-commands (append (list str) (reverse (cdr (reverse lispy-history-commands)))))
   ))
  (setq lispy-history-cursor -1))

(defun lispy-history-previous-entry ()
    (if (< lispy-history-cursor (- (length lispy-history-commands) 1))
        (setq lispy-history-cursor (+ lispy-history-cursor 1)))
    (if (>= lispy-history-cursor 0)
        (nth lispy-history-cursor lispy-history-commands)
      nil))

(defun lispy-history-next-entry ()
    (if (>= lispy-history-cursor 0)
        (setq lispy-history-cursor (- lispy-history-cursor 1)))
    (if (>= lispy-history-cursor 0)
        (nth lispy-history-cursor lispy-history-commands)
      nil))

(defun lispy-history-clear ()
  (setq lispy-history-commands nil
        lispy-history-cursor -1))

(defun lispy-history-back ()
  (interactive)
  (if (equal -1 lispy-history-cursor)
      (setq lispy-current-line (buffer-string)))
  (lispy-clear-text-area)
  (insert (let ((s (lispy-history-previous-entry)))
            (if (null s)
                lispy-current-line s
                ))))

(defun lispy-history-forward ()
  (interactive)
  (lispy-clear-text-area)
  (insert (let ((s (lispy-history-next-entry)))
            (if (null s)
                lispy-current-line s
                ))))

(define-key lispy-send-mode-map "\M-p" 'lispy-history-back)
(define-key lispy-send-mode-map "\M-n" 'lispy-history-forward)

(add-hook 'lispy-post-send-hook 'lispy-history-add-entry)
(add-hook 'lispy-connected-hook 'lispy-history-clear t)

(provide 'lispy-history)
;;; lispy-history.el ends here
