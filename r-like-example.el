;;; r-like-example.el --- Display examples -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2012

;; Author:  pogin <pogin503@gmail.com>
;; Keywords: lisp
;; Version: 0.1
;; URL: https://github.com/pogin503/r-like-example
;; Package-Requires: ((emacs "25.1") (f "0.16.2") (cl-lib "0.5"))

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
;;; Commentary:

;;; Installation:
;;
;; (require 'r-like-example)
;; (ex-set-keybindings)
;; (ex-load-examples)

;; Show examples of Emacs Lisp function.
;;
;;   (ex-example 'setq)
;;
;;; Code:

(require 'f)
(require 'cl-lib)
(require 'eldoc)
(require 'elisp-mode)

(defvar ex-hash (make-hash-table :test #'equal)
  "Store example.")

(defcustom ex-separator "\n;;=================================\n"
  "For separator of *example* buffer."
  :group 'r-like-example)

(defcustom ex-begin-comment ";;=> "
  "Beginning of output comment style."
  :group 'r-like-example)

(defcustom ex-examples-dir (expand-file-name "examples" (f-dirname (f-this-file)))
  "Examples directories."
  :group 'r-like-example)

(defconst ex-buffer-name "*example*" "Example output buffer For r-like-exaple.")
(defconst ex-debug-buffer-name "*example debug*")

(defun ex--before-def ()
  (progn
    (defun square (x) (* x x))
    (defalias 'even? (symbol-function 'cl-evenp))))

(ex--before-def)

;; Utility
(cl-defun ex-put-example (symbol examples &optional force (hash ex-hash))
  "Put examples function.

`SYMBOL' is key.
`EXAMPLE' is executable sexp."
  (cond  ((null examples)
          (ex-set-example symbol examples hash))
         ((eq force t)
          (if (and (not (null examples))
                   (atom examples))
              (ex-set-example symbol (list examples) hash)
            (ex-set-example symbol examples hash)))
         ((and (not (listp examples))
               (= (length (ex-get-example symbol hash)) 0))
          (puthash (symbol-name symbol) (list examples) hash))
         (t
          (let ((put-func
                 (lambda (ex)
                   (puthash (symbol-name symbol)
                            (reverse (cons ex
                                           (reverse
                                            (ex-get-example symbol hash))))
                            hash))))
            (if (and (not (null examples))
                     (atom examples))
                (funcall put-func examples)
              (mapc (lambda (ex)
                      (funcall put-func ex))
                    examples))))))

(cl-defun ex-get-example (symbol &optional (hash ex-hash))
  "Get exmaple function.

`SYMBOL' is key."
  (gethash (symbol-name symbol) hash))

(cl-defun ex-set-example (symbol example &optional (hash ex-hash))
  (puthash (symbol-name symbol) example hash))

(defun ex-eval-string (str)
  "Read sexp from string.

Example:
   (ex-eval-string \"(setq foo 1)\")
`STR' is sexp."
  (eval (with-temp-buffer
          (insert str)
          (read (buffer-string)))))

(defconst ex-novalue (make-symbol "<nil>")
  "Hash is no value symbol.")

(defun ex-hash-key-exists-p (key hash)
  (not (equal (gethash key hash ex-novalue) ex-novalue)))

(defun ex-query-key-exists (key &optional nomessage)
  (interactive "MQuery key : ")
  (let ((q (ex-hash-key-exists-p key ex-hash)))
    (if (null nomessage)
        (if q
            (message "%s: exists" key)
          (message "%s: not exists" key))
      q)))

;;;###autoload
(defun ex-example (symbol)
  "Print example to *example* buffer.

`SYMBOL' is function or varibale."
  (interactive "aFunction name is ? ")
  (when (or (stringp  symbol)
            (fboundp 'symbol)
            (boundp  'symbol)
            (symbolp 'symbol))
    (let ((buf (get-buffer-create ex-buffer-name)))
      (pop-to-buffer buf)
      (lisp-interaction-mode)
      (goto-char (point-min))
      (ex-insert-example symbol))))

(defun ex-get-example-result-string (symbol ex)
  (with-temp-buffer
    (condition-case err
        (let ((ex1 (ex-eval-string ex)))
          (cond  ((stringp ex1)
                  (insert (format "\"%s\"\n" ex1)))
                 (t
                  (insert (format "%S\n" ex1))))
          (goto-char (point-min))
          (forward-line 1)
          (while (null (eobp))
            (insert ";; ")
            (forward-line 1)))
      ((void-function void-variable)
       (insert (format "symbol: %s\nexample: %s\nerror: %s\n"
                       symbol ex (error-message-string err)))
       (message (format "symbol: %s\nexample: %s\nerror: %s\n"
                        symbol ex (error-message-string err)))))
    (buffer-string)))

(defun ex--get-fnsym-args-string (symbol)
  (let* ((eldoc-result (elisp-get-fnsym-args-string symbol))
         (func-args (if eldoc-result
                        (substring-no-properties eldoc-result)
                      (format "%s: ()" symbol))))
    (format ";; %s\n" func-args)))

(defun ex-insert-example (symbol)
  (insert (format ";; %s examples\n" symbol))
  (insert (ex--get-fnsym-args-string symbol))
  (mapc #'(lambda (ex)
            (insert (format "%s\n" ex))
            (insert ex-begin-comment)
            (with-current-buffer ex-buffer-name
              (insert (ex-get-example-result-string symbol ex))))
        (ex-get-example symbol))
  (insert ex-separator))

(defun ex-get-sexp-symbol-at-point ()
  "This function gets sexp symbol name on current position."
  ;; (interactive)
  (let* ((sym-string (substring-no-properties (thing-at-point 'sexp))))
    (with-temp-buffer
      (insert sym-string)
      (read (buffer-string)))))

;;;###autoload
(defun ex-put-to-example (key)
  "任意の関数のキーに、カーソル下のS式を追加する."
  (interactive "aどのシンボルに追加しますか? ")
  (forward-char 1)
  (beginning-of-defun)
  (let (beg end)
    (setq beg (point))
    (end-of-defun)
    (setq end (set-marker (make-marker) (point)))
    (goto-char end)
    (forward-char -1) ; remove newline
    (let ((ex (format "%s" (buffer-substring-no-properties beg (point)))))
      (ex-put-example key ex))))

(defun ex-example-data (key)
  "キーにストアされている実行例のstringを取得する.

`KEY' is function name."
  (with-temp-buffer
    (insert (format  "(ex-put-example '%s '(" key))
    (mapc #'(lambda (ex)
              (insert (format "%S\n" ex)))
          (ex-get-example key))
    (insert "))\n")
    (buffer-string)))

(defcustom ex-examples-dir (f-join (f-dirname (f-this-file)) "examples")
  "Examples directory."
  :group 'r-like-example)


(defconst ex-data-filepath (f-join ex-examples-dir "elisp-examples.el"))

;;;###autoload
(defun ex-store-key-example (key)
  "`KEY'に対応する実行例を、データ保存用ファイルに永続化する."
  (interactive "aどのキーをストアしますか? ")
  (let* ((db-file ex-data-filepath)
         (text1 (f-read-text db-file))
         (text (with-temp-buffer
                 (insert (format "%s" text1))
                 (let ((q (concat "^(ex-put-example '" (symbol-name key) " ")))
                   (if (re-search-backward q nil t)
                       (progn (mark-defun)
                              (delete-region (point) (1- (mark)))
                              (deactivate-mark)
                              (insert "\n")
                              (ex-insert-current-buffer key))
                     (goto-char (point-max))
                     (forward-line -8) ;; over (provide 'r-like-example)
                     (insert "\n")
                     (ex-insert-current-buffer key)
                     (insert "\n")))
                 (buffer-string))))
    (f-write-text text 'utf-8 db-file)))

(defun ex-copy-defun ()
  "Copy function at point."
  (interactive)
  (unless (eobp)
    (forward-char 1))
  (end-of-defun)
  (re-search-backward ")")
  (let ((end (point)))
    (mark-defun)
    ;; (setq x 123)
    ;; |
    ;; (setq y 456)
    ;; カーソル位置が「|」の位置にあったとき、
    ;; 改行を含めないために一文字進める。
    (when (string-match (buffer-substring-no-properties (point) (+ 1 (point))) "
")
      (forward-char 1))
    ;; re-search-backwardで")"上にカーソルがあるので一つ進めた状態でnarrowする
    (narrow-to-region (region-beginning) (+ 1 end))
    (let (result)
      (setq result (buffer-substring-no-properties (region-beginning)
                                                   (region-end)))
      (widen)
      result)))

(defun ex-get-beg-defun-symbol ()
  "バッファの現在位置のトップレベルにあるシンボル名を取得する"
  (interactive)
  (beginning-of-defun)
  (forward-char 1)
  (ex-get-sexp-symbol))

;;;###autoload
(defun ex-add-example ()
  "Add example on cursor point."
  (interactive)
  (unless (eobp)
    (forward-char 1))
  (beginning-of-defun)
  (forward-char 1)
  (let ((ex-sym (ex-get-sexp-symbol-at-point))
        pos (beg -1) (end 0))
    (mark-defun)
    (setq beg (string-match "\n"
                            (buffer-substring-no-properties
                             (point)
                             (+ 1 (point)))))
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
                          (region-end)       ; point is end-of-buffer
                        (- (region-end) 1))) ; except bottom extra line
    (deactivate-mark)
    (let (ex)
      (setq ex (format "%s" (substring-no-properties (get-register ?r))))
      (unless (symbolp ex-sym)
        (error (format "Please move cursor to S exppression.\n(`!!')")))
      (ex-put-example ex-sym ex)
      (message "%S" ex))))

;; Utility
(defun ex-insert-current-buffer (sym)
  "Insert code snippet of example.
Example:
\(ex-put-exmaple 'car '(\"(car '(1 2 3))\"))

`SYM' is function."
  (interactive "a(insert-currrent-buffer) Symbol name is ? ")
  (insert (format "(ex-put-example '%s '(" sym))
  (mapc #'(lambda (ex) (insert (format "%S\n" ex))) (ex-get-example sym))
  (delete-char -1)
  (insert ") t)"))

(defun ex-delete-last-elem (sym)
  "Delete last element in function examples.

`SYM' is key."
  (interactive "aDelete Symbol is? ")
  (let*  ((ex (ex-get-example sym))
          (last-elem (car (reverse ex))))
    (ex-put-example sym (reverse (cdr (reverse ex))))
    (message "delete \"%s\"" last-elem)))

(defun ex--collect-symbol (path)
  (interactive)
  (let ((data (f-read-text path))
        result)
    (with-temp-buffer
      (insert data)
      (goto-char (point-min))
      (while (null (eobp))
        (let ((q (re-search-forward "^(ex-put-example '" nil t))
              (str (thing-at-point 'symbol)))
          (if q
              (push (with-temp-buffer
                      (insert str)
                      (buffer-substring-no-properties (point-min) (point-max)))
                    result)
            (goto-char (point-max))))))
    (sort result #'string<)))

(defsubst ex--all-hash-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  (let ((keys))
    (maphash #'(lambda (key _value) (push key keys)) hash-table)
    keys))

(defun ex--collect-unstored-data ()
  (let (result
        (file-data (ex--collect-symbol ex-data-filepath))
        (current-data (sort (ex--all-hash-keys ex-hash) #'string<)))
    (cl-loop for x in current-data do
             (cond ((null (member x file-data))
                    (push (make-symbol x) result))))
    result))

(defun ex-display-unstored-data ()
  "Display unstored functions."
  (interactive)
  (message (format "%s" (ex--collect-unstored-data))))

(defun ex-store-unsaved-keys ()
  (interactive)
  (let ((unsaved-data (ex--collect-unstored-data)))
    (if unsaved-data
        (progn
          (mapc 'ex-store-key-example unsaved-data)
          (message "done... %s" (format "%s" unsaved-data)))
      (message "nothing"))))

;; Debug
(defun ex--exec-all-examples (hash)
  (mapc (lambda (x) (ex-example (intern-soft x)))
        (ex--all-hash-keys hash)))

(defun ex-set-keybindings ()
  "Set r-like-example keybindings."
  (when (equal system-type 'darwin)
    (global-set-key (kbd "s-9") 'ex-example)
    (global-set-key (kbd "s-0") 'ex-store-key-example))
  (global-set-key (kbd "M-9") 'ex-example)
  (global-set-key (kbd "M-0") 'ex-store-key-example)
  (global-set-key (kbd "C-c 0 a") 'ex-add-example)
  (global-set-key (kbd "C-c 0 d") 'ex-delete-last-elem)
  (global-set-key (kbd "C-c 0 i") 'ex-insert-current-buffer)
  (global-set-key (kbd "C-c 0 p") 'ex-put-to-example)
  (global-set-key (kbd "C-c 0 u") 'ex-display-unstored-data))

(defun ex-load-examples ()
  "Load examples."
  (add-to-list 'load-path ex-examples-dir)
  (require 'elisp-examples))

(provide 'r-like-example)

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; r-like-example.el ends here
