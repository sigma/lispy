;;; lispy.el --- small client for <Mtp> Chat !

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
(require 'lispy-vars)

(require 'lispy-send)

(defun lispy-not-yet-connected (string)
  (cond
   ((string-match "^<Mtp> Welcome, \\(\\w+\\)*\\." string)
    (setq lispy-remote-user (match-string 1 string))
    (lispy-set-keywords)
    (lispy-font-lock)
    (run-hooks 'lispy-connected-hook)
    (remove-hook 'lispy-pre-insert-hook 'lispy-not-yet-connected))
   ))

(defun lispy-filter (proc string)
  (with-current-buffer lispy-buffer
    (let* ((string2 (concat lispy-insert-buffer (replace-regexp-in-string "\r" "" string)))
           (str (if lispy-require-end-of-line
                    (progn
                      (let ((pos (- (length string2) 1 (string-match "\n" (apply 'string (reverse (string-to-list string2)))))))
                        (setq lispy-insert-buffer (substring string2 pos nil))
                        (substring string2 0 pos)))
                  string2)))
      (mapcar (lambda (seq)
                (if (string-match (concat "^" (regexp-quote (car seq)) ".*") str)
                    (progn
                      (funcall (cdr seq))
                      (setq str (replace-regexp-in-string (regexp-quote (car seq)) "" str)))))
              lispy-telnet-sequences)
      (let ((strlist (split-string str "[\n]"))
            (inhibit-read-only t))
        (mapcar (lambda (s)
                  (let ((st (concat s "\n")))
                    (run-hook-with-args 'lispy-pre-insert-hook st)
                    (cond
                     ((eq lispy-insert-line t)
                      (if (< (point) (point-max))
                          (save-excursion
                            (goto-char (point-max))
                            (insert st))
                        (insert st)))
                     ((eq lispy-insert-line 'next)
                      (setq lispy-insert-line t)))
                    (run-hook-with-args 'lispy-post-insert-hook st)))
                strlist)))
    (if lispy-read-password
        (process-send-string lispy-process (concat (read-passwd "") "\n")))))

(defun lispy-sentinel (process event)
  (message
   (format "Process: %s had the event `%s'" process event)
   (run-hooks 'lispy-disconnected-hook)))

;;;###autoload
(defun lispy (host port &optional buffer)
  (interactive (list (read-from-minibuffer "Telnet host: " lispy-default-host)
                     (read-from-minibuffer "Port: " lispy-default-port)))
  (let* ((buffer-name lispy-buffer-name)
	 proc)

    (setq buffer (get-buffer-create buffer-name))
    (switch-to-buffer buffer-name)
    (setq buffer-read-only t)
    (add-hook 'lispy-pre-insert-hook 'lispy-not-yet-connected)
    (setq lispy-host host lispy-port port lispy-remote-user nil lispy-buffer buffer)

    (set-process-filter
     (setq lispy-process (open-network-stream "Mtp" buffer-name host port))
     'lispy-filter)
    (set-process-sentinel lispy-process 'lispy-sentinel)
    (lispy-mode)))

(defun lispy-mode (&optional sub)
  "Set major-mode for mtp sessions.
If `lispy-mode-hook' is set, run it."
  (interactive)
  (mapcar 'make-local-variable '(
                                        ;lispy-host lispy-port lispy-remote-user lispy-buffer
                                        ;lispy-read-password lispy-read-user-list
                                        ;lispy-user-list
                                 lispy-insert-line
                                 lispy-require-end-of-line lispy-insert-buffer
                                 lispy-send-prefix))
  (setq major-mode 'lispy-mode)
  (setq mode-name "mtp")
  (use-local-map
   (cond
    ((eq sub 'send)
     lispy-send-mode-map)
    ((eq sub 'occur)
     lispy-occur-mode-map)
    (t lispy-mode-map)))
  (run-hooks 'lispy-mode-hook))

;; Definitions for keybindings

(setq lispy-mode-map (make-keymap))
(define-key lispy-mode-map "\C-m" 'lispy-reach-sending-buffer)
(substitute-key-definition 'self-insert-command (lambda () (interactive)
                                                  (lispy-reach-sending-buffer)
                                                  (insert (this-command-keys))) lispy-mode-map global-map)

(require 'lispy-commands)
(define-key lispy-mode-map "\C-xk" (lambda () (interactive)
                                     (if (and (not (null lispy-process)) (eq (process-status lispy-process) 'open))
                                         (lispy-quit))
                                     (run-hooks 'lispy-exit-hook)))

;; history
(require 'lispy-history)

;; font lock
(require 'lispy-font-lock)

(add-hook 'lispy-connected-hook (lambda ()
                                  (setq lispy-require-end-of-line t)
                                  (turn-on-font-lock)
                                  (lispy-message (format "set client %s\n" lispy-version))))
(add-hook 'lispy-disconnected-hook (lambda ()
                                  (setq lispy-require-end-of-line nil)))
(add-hook 'lispy-exit-hook (lambda ()
                             (kill-buffer lispy-buffer)))

(require 'lispy-occur)

(require 'lispy-session)

(provide 'lispy)
;;; lispy.el ends here