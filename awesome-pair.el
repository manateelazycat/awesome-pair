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

(defun awesome-pair-wrap-round ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (let ((string-bound (awesome-pair-string-start+end-points)))
           (save-excursion
             (goto-char (car string-bound))
             (insert "(")
             (goto-char (+ (cdr string-bound) 2))
             (insert ")"))))
        ((awesome-pair-in-comment-p)
         (save-excursion
           (let ((start (beginning-of-thing 'symbol))
                 (end (end-of-thing 'symbol)))
             (goto-char start)
             (insert "(")
             (goto-char (+ end 1))
             (insert ")"))))
        (t
         (save-excursion
           (let ((start (beginning-of-thing 'sexp))
                 (end (end-of-thing 'sexp)))
             (goto-char start)
             (insert "(")
             (goto-char (+ end 1))
             (insert ")"))
           ))))

(defun awesome-pair-wrap-bracket ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (let ((string-bound (awesome-pair-string-start+end-points)))
           (save-excursion
             (goto-char (car string-bound))
             (insert "[")
             (goto-char (+ (cdr string-bound) 2))
             (insert "]"))))
        ((awesome-pair-in-comment-p)
         (save-excursion
           (let ((start (beginning-of-thing 'symbol))
                 (end (end-of-thing 'symbol)))
             (goto-char start)
             (insert "[")
             (goto-char (+ end 1))
             (insert "]"))))
        (t
         (save-excursion
           (let ((start (beginning-of-thing 'sexp))
                 (end (end-of-thing 'sexp)))
             (goto-char start)
             (insert "[")
             (goto-char (+ end 1))
             (insert "]"))
           ))))

(defun awesome-pair-wrap-curly ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (let ((string-bound (awesome-pair-string-start+end-points)))
           (save-excursion
             (goto-char (car string-bound))
             (insert "{")
             (goto-char (+ (cdr string-bound) 2))
             (insert "}"))))
        ((awesome-pair-in-comment-p)
         (save-excursion
           (let ((start (beginning-of-thing 'symbol))
                 (end (end-of-thing 'symbol)))
             (goto-char start)
             (insert "{")
             (goto-char (+ end 1))
             (insert "}"))))
        (t
         (save-excursion
           (let ((start (beginning-of-thing 'sexp))
                 (end (end-of-thing 'sexp)))
             (goto-char start)
             (insert "{")
             (goto-char (+ end 1))
             (insert "}"))
           ))))

(defun awesome-pair-wrap-double-quote ()
  (interactive)
  (cond ((awesome-pair-in-string-p)
         (goto-char (1+ (cdr (awesome-pair-string-start+end-points)))))
        ((awesome-pair-in-comment-p)
         (save-excursion
           (let ((start (beginning-of-thing 'symbol))
                 (end (end-of-thing 'symbol)))
             (goto-char start)
             (insert "\"")
             (goto-char (+ end 1))
             (insert "\""))))
        (t
         (save-excursion
           (let ((start (beginning-of-thing 'sexp))
                 (end (end-of-thing 'sexp)))
             (goto-char start)
             (insert "\"")
             (goto-char (+ end 1))
             (insert "\""))))))

(defun awesome-pair-splice-sexp (&optional argument)
  (interactive "P")
  (if (awesome-pair-in-string-p)
      (awesome-pair-splice-string argument)
    (save-excursion
      (awesome-pair-kill-surrounding-sexps-for-splice argument)
      (backward-up-list)
      (save-excursion
        (forward-sexp)
        (backward-delete-char 1))
      (delete-char 1)
      (ignore-errors
        (backward-up-list)
        (indent-sexp)))))

(defun awesome-pair-splice-string (argument)
  (let ((original-point (point))
        (start+end (awesome-pair-string-start+end-points)))
    (let ((start (car start+end))
          (end (cdr start+end)))
      (let* ((escaped-string
              (cond ((not (consp argument))
                     (buffer-substring (1+ start) end))
                    ((= 4 (car argument))
                     (buffer-substring original-point end))
                    (t
                     (buffer-substring (1+ start) original-point))))
             (unescaped-string
              (awesome-pair-unescape-string escaped-string)))
        (if (not unescaped-string)
            (error "Unspliceable string.")
          (save-excursion
            (goto-char start)
            (delete-region start (1+ end))
            (insert unescaped-string))
          (if (not (and (consp argument)
                        (= 4 (car argument))))
              (goto-char (- original-point 1))))))))

(defun awesome-pair-point-at-sexp-start ()
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (point)))

(defun awesome-pair-point-at-sexp-end ()
  (save-excursion
    (backward-sexp)
    (forward-sexp)
    (point)))

(defun awesome-pair-point-at-sexp-boundary (n)
  (cond ((< n 0) (awesome-pair-point-at-sexp-start))
        ((= n 0) (point))
        ((> n 0) (awesome-pair-point-at-sexp-end))))

(defun awesome-pair-kill-surrounding-sexps-for-splice (argument)
  (cond ((or (awesome-pair-in-string-p)
             (awesome-pair-in-comment-p))
         (error "Invalid context for splicing S-expressions."))
        ((or (not argument) (eq argument 0)) nil)
        ((or (numberp argument) (eq argument '-))
         (let* ((argument (if (eq argument '-) -1 argument))
                (saved (awesome-pair-point-at-sexp-boundary (- argument))))
           (goto-char saved)
           (ignore-errors (backward-sexp argument))
           (awesome-pair-hack-kill-region saved (point))))
        ((consp argument)
         (let ((v (car argument)))
           (if (= v 4)
               (let ((end (point)))
                 (ignore-errors
                   (while (not (bobp))
                     (backward-sexp)))
                 (awesome-pair-hack-kill-region (point) end))
             (let ((beginning (point)))
               (ignore-errors
                 (while (not (eobp))
                   (forward-sexp)))
               (awesome-pair-hack-kill-region beginning (point))))))
        (t (error "Bizarre prefix argument `%s'." argument))))

(defun awesome-pair-unescape-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (not (eobp))
                (search-forward "\\" nil t))
      (delete-char -1)
      (forward-char))
    (condition-case condition
        (progn (check-parens) (buffer-string))
      (error nil))))

(defun awesome-pair-hack-kill-region (start end)
  (interactive "r")
  (let ((this-command nil)
        (last-command nil))
    (kill-region start end)))

(provide 'awesome-pair)

;;; awesome-pair.el ends here
