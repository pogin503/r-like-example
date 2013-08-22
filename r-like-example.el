;;; r-like-example.el --- r-like-example

;; Copyright (C) 2012

;; Author:  <pogin>
;; Keywords: lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;
;;; Installation:
;;
;; (require 'r-like-example)
;;

;;; Commentary:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar ex-hash (make-hash-table :test #'equal)
  "Store example.")

(defcustom ex-separator "\n;=================================\n\n"
  "For separator of *example* buffer."
  :group 'r-like-example)

(defun ex-put-example (symbol example)
  "Get examples function."
  (puthash (symbol-name (eval 'symbol)) example ex-hash)
  )

;; (defmacro ex-put-example-macro-1 (symbol example)
;;   `(puthash (symbol-name ',symobl) ,(quote example) ex-hash))

;; (defun ex-put-example-macro (symbol example)
;;   (ex-put-example-macro-1 symbol example))

(defun ex-get-example (symbol)
  "Get exmaple function."
  (gethash (symbol-name (eval 'symbol)) ex-hash))

(defun ex-eval-string (str)
  "Read sexp from string.

Example:
   (ex-eva-string \"(setq foo 1)\")
`STR' is sexp."
  (eval (with-temp-buffer
          (insert str)
          (read (buffer-string)))))

(defconst ex-novalue (make-symbol "<nil>"))

(defun ex-hash-exists-p (key table)
    (not (eq (gethash key table ex-novalue) ex-novalue)))

(defconst ex-buffer-name "*example*")

(defun ex-example (symbol)
  "Print example to *example* buffer."
  (interactive "aSymbol name? ")
  ;; (ex-kill-ex-buffer)
  (when (if (stringp symbol)
            (read symbol)
          (fboundp (eval 'symbol)))
    (let ((buf (get-buffer-create ex-buffer-name)))
      (get-buffer buf)
      (pop-to-buffer buf)
      (lisp-interaction-mode)
      (goto-char (point-min))
      (ex-insert-example symbol)
    )))

(defun ex-examples (symbols)
  (interactive)
  (let ((buf (get-buffer-create ex-buffer-name))
        (sep ex-separator)
        )
    (get-buffer buf)
    (pop-to-buffer buf)
    (mapcar #'ex-insert-example symbols)))

(defun ex-insert-example (symbol)
  (goto-char (point-min))
  (mapc #'(lambda (ex)
              (insert (format "%s\n" ex))
              (insert ";=> ")
              (save-excursion
                (condition-case err
                    (let ((ex1 (ex-eval-string ex)))
                      (cond  ((stringp ex1)
                              (with-current-buffer ex-buffer-name
                                (insert (format "\"%s\"\n" ex1))))
                             (t (with-current-buffer ex-buffer-name
                                  (insert (format "%s\n" ex1))))))
                  ((void-function void-variable)
                   (insert (format "%s" (error-message-string err)))))
                )
              (forward-line 1)
              ) (ex-get-example (eval 'symbol)))
  (insert ex-separator)
  )

(defun ex-get-sexp-symbol ()
  "This function gets sexp symbol name on current position."
  ;; (interactive)
  (let* ((sym-string (substring-no-properties (thing-at-point 'sexp)))
        (sym (with-temp-buffer
               (insert sym-string)
               (read (buffer-string)))))
    sym
  ))

(defun ex-add-example ()
  (interactive)
  (forward-char 1)
  (beginning-of-defun)
  (forward-char 1)
  (let ((ex-sym (ex-get-sexp-symbol))
        pos (beg -1) (end 0) ex)
    (mark-defun)
    (setq beg (string-match "\n" (buffer-substring-no-properties (point) (+ 1 (point)))))
    ;; (when (equal beg 0) (forward-char 1))
    (save-excursion
      (end-of-defun)
      (setq pos (point))
      (goto-char (point-max))
      ;; whether cursor position is end of buffer
      (setq end (or (= pos (point))
                    (= pos (+ (point) 1)))))
    (copy-to-register ?r
                      (if (equal beg 0)
                          (+ 1 (region-beginning)) ;; except top extra line
                        (region-beginning))
                      (if end
                          (region-end)            ;; point is end-of-buffer
                        (- (region-end) 1)))      ;; except bottom extra line
    (setq ex (format "%s" (substring-no-properties (get-register ?r))))
    (cond  ((= (length (ex-get-example ex-sym)) 0)
            (ex-put-example ex-sym (list ex)))
           (t
            (ex-put-example ex-sym (reverse (cons ex (reverse (ex-get-example ex-sym)))))))
    (message ex)
    ;; (ex-put-example ex-sym (destructuring-bind ((a . b) c)
    ;;                            `(,(ex-get-example ex) ,(format "%s" (substring-no-properties ex)))
    ;;                          (list a b c)
    ;;                          ))
    ;; (ex-put-example ex-sym (ex-get-example ex-sym)
    ;;                                 (format "%s" (substring-no-properties (get-register ?r)))))
    ;; (message (format "%s" (ex-get-example ex-sym)))
    ))

;; Utility
(defun ex-insert-current-buffer (sym)
  "Insert code snippet of example.
Example:
\(ex-put-exmaple 'car '(\"(car '(1 2 3))\"))

`SYM' is function."
  (interactive "aSymbol name? ")
  (insert (format "(ex-put-example '%s '(" sym))
  (mapc #'(lambda (ex) (insert (format "%S\n" ex))) (ex-get-example sym))
  (delete-char -1)
  (insert "))")
  ;; (insert (format "%S" (ex-get-example sym)))
  )
;; (global-set-key (kbd "s-0") 'ex-insert-current-buffer)

(defun ex-delete-last-elem (sym)
  "Delete last element in function examples."
  (interactive "aDelete Symbol is? ")
  ;; (ex-put-example 'car '("(car '(1 2 3))") ex-test-hs)
  (let  ((ex (ex-get-example sym)))
    (ex-put-example sym (reverse (cdr (reverse ex))))))
;; (global-set-key (kbd "C-c 9") 'ex-delete-last-elem)

;; Window
(defun ex-delete-window ()
  (interactive)
  (let ((win (get-buffer-window ex-buffer-name)))
    (delete-window win)))

(defvar ex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'ex-delete-window)
    map))

(defun ex-kill-ex-buffer ()
  (if (get-buffer ex-buffer-name)
      (kill-buffer ex-buffer-name)))

;; (define-derived-mode ex-mode nil "Example"
;;   ""
;;   (setq buffer-read-only t)
;;   (use-local-map ex-mode-map))
;; (defun ex-dump-example (symbol)
;;   (maphash #')
;;   )

(provide 'r-like-example)

;;; r-like-example ends here
