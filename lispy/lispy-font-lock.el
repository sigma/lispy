;;; lispy-font-lock.el ---

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

(defvar lispy-keywords '())

(defface lispy-default-face '((t (:foreground "wheat"))) "")
(defface lispy-aboutme-face '((t (:foreground "lightblue"))) "")
(defface lispy-Mtp-face '((t (:foreground "yellow"))) "")
(defface lispy-me-face '((t (:foreground "grey"))) "")
(defface lispy-tell-face '((t (:foreground "red"))) "")
(defface lispy-url-face '((t (:foreground "green"))) "")

(defvar lispy-default-face 'lispy-default-face)
(defvar lispy-aboutme-face 'lispy-aboutme-face)
(defvar lispy-Mtp-face 'lispy-Mtp-face)
(defvar lispy-me-face 'lispy-me-face)
(defvar lispy-tell-face 'lispy-tell-face)
(defvar lispy-url-face 'lispy-url-face)

(defun lispy-set-keywords ()
  (let ((user (lispy-case-unsensitive lispy-remote-user)))
    (setq lispy-keywords `(
                           ("^\\(|.*\\)$" 1 lispy-tell-face)
                           (,(concat "^\\(<" user ">.*\\)$") 1 lispy-me-face)
                           (,(concat "^\\(.*" user ".*\\)$") 1 lispy-aboutme-face)
                           ("^\\(<Mtp>\\s-\\sw+\\s-tells\\s-.*\\)$" 1 lispy-tell-face)
                           ("^\\(<Mtp>\\s-You\\s-.*\\)$" 1 lispy-me-face)
                           ("^\\(<Mtp>.*\\)$" 1 lispy-Mtp-face)
                           ("\\(http://[^ \n()]*\\).*$" 1 lispy-url-face)
                           ))))

(defun lispy-font-lock ()
  "Turn on font-lock for Mtp keywords."
  (interactive)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(lispy-keywords t nil nil nil))
)

(defun lispy-font-lock-hook ()
  (if (eq major-mode 'lispy-mode)
      (lispy-font-lock)))

(add-hook 'font-lock-mode-hook 'lispy-font-lock-hook)

(add-hook 'lispy-connected-hook (lambda ()
                                  (lispy-set-keywords)
                                  (lispy-font-lock)
                                  (turn-on-font-lock)))


(provide 'lispy-font-lock)
;;; lispy-font-lock.el ends here
