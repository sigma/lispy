;;; lispy-session.el --- Use symbolic names for you sessions

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

;; use something like
;; (setq lispy-session-alist '(("name1" "host1" "port1" "login1" "password1")
;;                             ("name2" "host2" "port2" "login2" "password2")))
;;
;; login and password are optional.

;;; Code:

(require 'lispy-vars)
(require 'lispy-utils)

(defvar lispy-session-alist '(("zen" "zen.dtdns.net" 4000)) "Sessions name to be used when launching `lispy'." )

(lispy-defvar lispy-session-current-session nil)

(defadvice lispy (around around-lispy-session activate)
  (interactive (let ((elem (assoc
                            (completing-read "Session: "
                                             (let ((i -1))
                                               (mapcar (lambda (e) (list (car e) (incf i)))
                                                       lispy-session-alist)))
                            lispy-session-alist)))
                 (lispy-add-hook-once 'lispy-mode-hook `(lambda ()
                                                          (setq lispy-session-current-session ',elem)))
                 (list (nth 1 elem) (nth 2 elem))))
  ad-do-it
  (when (nth 3 lispy-session-current-session)
    (lispy-message (nth 3 lispy-session-current-session))))

(defadvice lispy-read-password (around around-lispy-read-password activate)
  (let ((lispy-password (nth 4 lispy-session-current-session)))
    ad-do-it))

(defun lispy-session (s)
  (let ((elem (if (stringp s)
                  (assoc s lispy-session-alist)
                s)))
    (lispy-add-hook-once 'lispy-mode-hook `(lambda ()
                                             (setq lispy-session-current-session ',elem)))
    (lispy (nth 1 elem) (nth 2 elem))))

;; (defadvice lispy-reconnect (after after-lispy-reconnect activate)
;;   (when (nth 3 lispy-session-current-session)
;;     (lispy-message (nth 3 lispy-session-current-session))))

(provide 'lispy-session)
;;; lispy-session.el ends here
