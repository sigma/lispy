;;; lispy-session.el ---

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

(defvar lispy-session-alist '(("zen" "zen.dtdns.net" 4000)
                              ("home" "sigmamtp.dyndns.org" 5000)
                              ("local" "localhost" 5000)) "")

(fset 'lispy
      `(lambda (elem)
         (interactive (list (assoc
                             (completing-read "Session: "
                                              (let ((i -1))
                                                (mapcar (lambda (e) (list (car e) (incf i)))
                                                        lispy-session-alist)))
                             lispy-session-alist)))
         (funcall ,(symbol-function 'lispy) (nth 1 elem) (nth 2 elem))))

(provide 'lispy-session)
;;; lispy-session.el ends here
