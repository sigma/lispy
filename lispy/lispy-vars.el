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

(defvar lispy-version "Lispy 0.3"
  "*Version of the program")

(defvar lispy-mode-hook nil "*Hook to run after setting current buffer to lispy-mode.")
(defvar lispy-pre-insert-hook nil "Hook to run before inserting a new line. Functions are called with the string as argument")
(defvar lispy-post-insert-hook nil "Hook to run after inserting a new line. Functions are called with the string as argument")
(defvar lispy-pre-send-hook nil "")
(defvar lispy-post-send-hook nil "")
(defvar lispy-connected-hook nil "")
(defvar lispy-disconnected-hook nil "")
(defvar lispy-exit-hook nil "")

(defvar lispy-default-host "localhost")
(defvar lispy-default-port "5000")

(defvar lispy-host nil)
(defvar lispy-port nil)
(defvar lispy-remote-user nil "")
(defvar lispy-read-password nil "")
(defvar lispy-read-user-list nil "")
(defvar lispy-user-list nil "")
(defvar lispy-insert-line t "")
(defvar lispy-require-end-of-line nil "")
(defvar lispy-insert-buffer nil "")

(defvar lispy-mode-map '())
(defvar lispy-send-mode-map '())

(defvar lispy-process nil)
(defvar lispy-buffer nil)
(defvar lispy-buffer-name "*<Mtp> Chat*")
(defvar lispy-send-buffer nil)
(defvar lispy-send-buffer-name "*<Mtp> Chat* -- send")
(defvar lispy-send-buffer-height 5)
(defvar lispy-send-prefix "")
(defvar lispy-telnet-sequences '(
                                 ("��" . lispy-turn-off-echo)
                                 ("��" . lispy-turn-on-echo)
                                 ("" . lispy-beep)
                                 ("[2J[0;0H" . lispy-clear-text-area)) "")

(provide 'lispy-vars)
;;; lispy-vars.el ends here
