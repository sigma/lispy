;;; lispy-occur.el ---

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

(defvar lispy-occur-mode-map '())

(defvar lispy-occur-buffers nil "")

(defun lispy-occur (regexp buffername prefix &optional filter)
  (interactive
   (list (let* ((default (car regexp-history))
		(input
		 (read-from-minibuffer
		  (if default
		      (format "List lines matching regexp (default `%s'): "
			      default)
		    "List lines matching regexp: ")
		  nil nil nil 'regexp-history default t)))
	   (and (equal input "") default
		(setq input default))
	   input)
         (read-from-minibuffer "Buffer Name: ")
         ""))
  (let ((base-buffer (current-buffer)))
    (save-window-excursion
      (with-output-to-temp-buffer buffername
        (lispy-inherit-buffer-local-variables base-buffer (get-buffer buffername))
        (save-excursion
          (goto-char (point-min))
          (while (and (not (eobp))
                      (re-search-forward regexp nil t))
            (let* ((start (save-excursion
                            (beginning-of-line)
                            (point)))
                   (end (save-excursion
                          (end-of-line)
                          (point))))
              (save-excursion
                (set-buffer standard-output)
                (insert (funcall (or filter 'identity)
                               (with-current-buffer base-buffer
                                 (buffer-substring-no-properties start end))))
                (insert "\n")
                (goto-char (point-max)))
              (forward-line 1)))
          (set-buffer buffername)
          (lispy-mode 'occur)
          (lispy-font-lock)))))
  (switch-to-buffer buffername)
  ;; FIXME : why the hell is view-mode enabled?
  (view-mode -1)
  (setq buffer-read-only t)
  (setq lispy-send-prefix prefix)
  (add-to-list 'lispy-occur-buffers (list buffername regexp prefix filter)))

(defun lispy-occur-append (buffer string)
  (save-window-excursion
    (set-buffer buffer)
    (let ((inhibit-read-only t)
          (p (point)))
      (goto-char (point-max))
      (insert string)
      (goto-char p))))

(setq lispy-occur-mode-map (make-keymap))
(define-key lispy-occur-mode-map "\C-m" 'lispy-reach-sending-buffer)
(define-key lispy-occur-mode-map "\C-xk" (lambda ()
                                           (interactive)
                                           (setq lispy-occur-buffers (delete (assoc (buffer-name) lispy-occur-buffers) lispy-occur-buffers))
                                           (kill-this-buffer)))
(substitute-key-definition 'self-insert-command (lambda () (interactive)
                                                  (lispy-reach-sending-buffer)
                                                  (insert (this-command-keys))) lispy-occur-mode-map global-map)

(add-hook 'lispy-exit-hook (lambda ()
                             (mapcar (lambda (elem) (kill-buffer (car elem))) lispy-occur-buffers)
                             (setq lispy-occur-buffers nil)))
(add-hook 'lispy-pre-insert-hook (lambda (s)
                                   (mapcar (lambda (elem)
                                             (if (string-match (nth 1 elem) s)
                                                 (lispy-occur-append (nth 0 elem) (funcall (or (nth 3 elem) 'identity) s))))
                                           lispy-occur-buffers)))

(defun lispy-occur-tell (arg)
  (interactive "P")
  (let* ((login (if (null arg)
                    (completing-read "Login: " lispy-user-list)
                  ;;                   (read-from-minibuffer "Login: ")
                 (word-at-point)))
         (reg (format "^<Mtp> \\(You tell %s:\\|%s tells you:\\)" login login))
         (buf (format "*<Mtp> Tell %s*" login))
         (pref (format "tell %s " login)))
    (lispy-occur reg buf pref `(lambda (s)
                                (let ((st (replace-regexp-in-string (format "^<Mtp> You tell %s:" ,login) (format "<%s>" lispy-remote-user) s)))
                                  (replace-regexp-in-string (format "^<Mtp> %s tells you:" ,login) (format "<%s>" ,login) st))))))

(define-key lispy-mode-map "\C-c/t" 'lispy-occur-tell)

(define-key lispy-occur-mode-map "\C-c//" (lambda () (interactive) (switch-to-buffer lispy-buffer)))

(provide 'lispy-occur)
;;; lispy-occur.el ends here
