;;; lispy-send.el ---

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

(require 'lispy-utils)

(defun lispy-reach-sending-buffer ()
  (interactive)
  (if (or (null lispy-send-buffer)
          (not (buffer-live-p lispy-send-buffer)))
      (lispy-init-send-buffer))
  (let ((pref lispy-send-prefix))
    (pop-to-buffer lispy-send-buffer nil t)
    (shrink-window (- (window-height) lispy-send-buffer-height))
    (setq lispy-send-prefix pref)))

(defun lispy-init-send-buffer ()
  (save-window-excursion
    (let ((from (current-buffer))
          (to (switch-to-buffer lispy-send-buffer-name t)))
      (lispy-inherit-buffer-local-variables from to)
      (setq lispy-send-buffer to)
      (lispy-mode 'send))))

(setq lispy-send-mode-map (make-keymap))
(define-key lispy-send-mode-map "\C-m" 'lispy-send)
(define-key lispy-send-mode-map [\M-return] 'newline)
(define-key lispy-send-mode-map [tab] 'lispy-complete-nickname)

(add-hook 'lispy-post-message-hook (lambda (s)
                                  (if (eq (window-buffer) lispy-send-buffer)
                                      (delete-window))
                                  (goto-char (point-max))))
(add-hook 'lispy-exit-hook (lambda ()
                             (kill-buffer lispy-send-buffer)))

(provide 'lispy-send)
;;; lispy-send.el ends here
