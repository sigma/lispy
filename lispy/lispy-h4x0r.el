;;; lispy-h4x0r.el ---

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

(require 'h4x0r)

(eval
 (let ((lispy-h4x0r (lambda ()
                      (setq s (h4x0r-string s)))))

   `(define-minor-mode lispy-h4x0r-mode
      "A mode to send h4x0r sentences."
      nil " h4x0r" nil
      :group 'lispy
      (if lispy-h4x0r-mode
          (add-hook 'lispy-pre-send-hook ,lispy-h4x0r)
        (remove-hook 'lispy-pre-send-hook ,lispy-h4x0r)))))


(provide 'lispy-h4x0r)
;;; lispy-h4x0r.el ends here
