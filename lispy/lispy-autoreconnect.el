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

;;

;;; Code:

(require 'lispy-utils)
(require 'lispy-vars)

(lispy-defvar lispy-inhibit-reconnect nil "")

(defun lispy-reconnect ()
  (interactive)
  (unless lispy-inhibit-reconnect
    (progn
;;       (condition-case nil
;;           (progn
;;             (lispy-quit)
;;             (sit-for 1))
;;         (error nil))
      (let ((lispy-inhibit-reconnect t))
        (run-hooks 'lispy-disconnected-hook))
      (lispy lispy-host lispy-port lispy-buffer)
      (lispy-update-buffer-hierarchy (current-buffer) 'lispy-process)
      (goto-char (point-max)))))

(defadvice lispy-quit (before before-lispy-quit act)
    (setq lispy-inhibit-reconnect t)
    (lispy-update-buffer-hierarchy (current-buffer) 'lispy-inhibit-reconnect))

(define-key lispy-mode-map "\C-cR" 'lispy-reconnect)

(add-hook 'lispy-disconnected-hook 'lispy-reconnect)

(provide 'lispy-autoreconnect)
;;; lispy-autoreconnect.el ends here
