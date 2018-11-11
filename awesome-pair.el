;;; awesome-pair.el --- Auto parenthesis pairing with syntax table

;; Filename: awesome-pair.el
;; Description: Auto parenthesis pairing with syntax table
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-11-11 09:27:58
;; Version: 0.1
;; Last-Updated: 2018-11-11 09:27:58
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/awesome-pair.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Auto parenthesis pairing with syntax table.
;;
;; I'm a big fans of paredit.el, I have used the paredit.el more than ten years.
;; But paredit.el not very good for web programming, so I think it's time to write my own plugin.
;;
;; Thanks Taylor R. Campbell, you surprise me how wonderful Emacs is.
;;

;;; Installation:
;;
;; Put awesome-pair.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'awesome-pair)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET awesome-pair RET
;;

;;; Change log:
;;
;; 2018/11/11
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defun awesome-pair-open-round ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert "("))
        (t
         (insert "()")
         (backward-char))
        ))

(defun awesome-pair-open-curly ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert "{"))
        (t
         (insert "{}")
         (backward-char))
        ))

(defun awesome-pair-open-bracket ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert "["))
        (t
         (insert "[]")
         (backward-char))
        ))

(defun awesome-pair-close-round ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert ")"))
        (t
         (let ((close (awesome-pair-missing-close)))
           (if (and close
                    (eq ?\) (matching-paren close)))
               (insert ")")
             )))))

(defun awesome-pair-close-curly ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert "}"))
        (t
         (let ((close (awesome-pair-missing-close)))
           (if (and close
                    (eq ?\} (matching-paren close)))
               (insert "}")
             )))))

(defun awesome-pair-close-bracket ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert "]"))
        (t
         (let ((close (awesome-pair-missing-close)))
           (if (and close
                    (eq ?\] (matching-paren close)))
               (insert "]")
             )))))

(defun awesome-pair-jump-out-pair-and-newline ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (goto-char (1+ (cdr (awesome-pair-string-start+end-points))))
         (newline-and-indent))
        (t
         (up-list)
         (newline-and-indent))))

(defun awesome-pair-missing-close ()
  (let (open)
    (ignore-errors
      (save-excursion
        (backward-up-list)
        (setq open (char-after))
        (if (ignore-errors
              (forward-sexp)
              t)
            nil
          open)))))

(defun awesome-pair-double-quote ()
  (interactive)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (insert "\\\""))
        (t
         (insert "\"\"")
         (backward-char))
        ))

(defun awesome-pair-backward-delete ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (awesome-pair-backward-delete-in-string))
        ((awesome-pair-in-comment-p)
         (backward-delete-char 1))
        ((awesome-pair-after-close-pair-p)
         (awesome-pair-backward-movein-or-delete-close-pair))
        ((awesome-pair-in-empty-pair-p)
         (awesome-pair-backward-delete-in-pair))
        ((not (awesome-pair-after-open-pair-p))
         (backward-delete-char 1))))

(defun awesome-pair-backward-delete-in-pair ()
  (backward-delete-char 1)
  (delete-char 1))

(defun awesome-pair-backward-movein-or-delete-close-pair ()
  (if (ignore-errors
        (save-excursion (backward-sexp))
        t)
      (backward-char)
    (backward-delete-char 1)))

(defun awesome-pair-after-open-pair-p ()
  (let ((syn (char-syntax (char-before))))
    (or (eq syn ?\()
        (and (eq syn ?_)
             (eq (char-before) ?\{)))
    ))

(defun awesome-pair-after-close-pair-p ()
  (let ((syn (char-syntax (char-before))))
    (or (eq syn ?\) )
        (eq syn ?\" )
        (and (eq syn ?_ )
             (eq (char-before) ?\}))
        )))

(defun awesome-pair-in-empty-pair-p ()
  (or (and (eq (char-syntax (char-before)) ?\()
           (eq (char-after) (matching-paren (char-before))))
      (and (eq (char-syntax (char-before)) ?_)
           (eq (char-before) ?\{)
           (eq (char-syntax (char-after)) ?_)
           (eq (char-after) ?\})
           )))

(defun awesome-pair-in-string-p (&optional state)
  (and (nth 3 (or state (awesome-pair-current-parse-state)))
       t))

(defun awesome-pair-in-comment-p (&optional state)
  (and (nth 4 (or state (awesome-pair-current-parse-state)))
       t))

(defun awesome-pair-current-parse-state ()
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun awesome-pair-string-start+end-points (&optional state)
  (save-excursion
    (let ((start (nth 8 (or state (awesome-pair-current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun awesome-pair-in-string-escape-p ()
  (let ((oddp nil))
    (save-excursion
      (while (eq (char-before) ?\\ )
        (setq oddp (not oddp))
        (backward-char)))
    oddp))

(defun awesome-pair-backward-delete-in-string ()
  (let ((start+end (awesome-pair-string-start+end-points)))
    (cond ((not (eq (1- (point)) (car start+end)))
           (if (awesome-pair-in-string-escape-p)
               (delete-char 1))
           (backward-delete-char 1)
           (if (awesome-pair-in-string-escape-p)
               (backward-delete-char 1)))
          ((eq (point) (cdr start+end))
           (backward-delete-char 1)
           (delete-char 1)))))

(provide 'awesome-pair)

;;; awesome-pair.el ends here
