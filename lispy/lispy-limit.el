;;; lispy-limit.el --- introduce some limits for lispy

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

;; control the amount of text that is kept in the buffer. Written for
;; Erbot, you probably don't need it.

;;; Code:

(defvar lispy-max-lines-number 500)
(defvar lispy-min-lines-number 300)

(defun lispy-ensure-max-buffer-size ()
  (let ((lines (line-number-at-pos (point-max))))
    (when (> lines lispy-max-lines-number)
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-line (- lines lispy-min-lines-number))
          (beginning-of-line)
          (delete-region (point-min) (point)))))))

(add-hook 'lispy-post-insert-hook (lambda (&rest args)
                                    (lispy-ensure-max-buffer-size)))

(provide 'lispy-limit)
;;; lispy-limit.el ends here
