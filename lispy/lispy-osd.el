;;; lispy-osd.el ---

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

(require 'osd)

(defvar lispy-osd-patterns nil "")

(osd-init)

(defun lispy-osd-install-login-patterns ()
  (add-to-list 'lispy-osd-patterns lispy-remote-user)
  )

(add-hook 'lispy-pre-insert-hook (lambda (str)
                                   (mapcar (lambda (pat)
                                             (if (string-match pat str)
                                                 (osd-display str)))
                                           lispy-osd-patterns)))

(add-hook 'lispy-connected-hook 'lispy-osd-install-login-patterns)

(add-hook 'lispy-exit-hook 'osd-close)

(provide 'lispy-osd)
;;; lispy-osd.el ends here
