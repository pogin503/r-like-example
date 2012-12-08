;;; r-like-example.el --- r-like-example

;; Copyright (C) 2012

;; Author:  <pogin>
;; Keywords: lisp

;; TODO
;; ex-exampleを作る
;; バッファに出力する -> OK
;; バッファの作り方を調べる -> OK?
;; popup できるようにする -> OK
;; popupの内容のクリア -> OK
;; hashのデータテーブルの改善
;; popwinのデバッグ設定を作る -> OK
;; 関数名の出力と実行結果の出力を作る


(defcustom ex-debug t
  "Enable debug message"
  :type 'boolean
  :group 'ex)

(if ex-debug
    (progn
      (defun ex-foo ()
        t)
      (ex-put-example 'ex-foo "(ex-foo)")
      (push '("*example*" :position bottom) popwin:special-display-config)))

(defconst ex-hash (make-hash-table :test #'equal)
  "hash")

(defun ex-put-example (symbol example)
  (puthash (symbol-name (eval 'symbol)) example ex-hash)
  )

;; (puthash "example" "(defun ex-test () nil) (example 'ex-test)" ex-hash)
;; (puthash "example" '("(defun ex-test () nil)"  "(example 'ex-test)") ex-hash)
;; (ex-put-example 'example "(defun ex-test () nil)  (example 'ex-test)")

(defun ex-get-example (symbol)
  (gethash (symbol-name (eval 'symbol)) ex-hash))
;; (ex-get-example 'example)


(defun eval-string (str)
  (eval (with-temp-buffer
          (insert str)
          (read (buffer-string)))))

;; (defun ex-eval-string (str)
;;   (with-temp-buffer
;;     (get-buffer-create "*example*")
;;     (mapcar #'(lambda (x)
;;                 (eval (with-temp-buffer
;;                         (insert str)
;;                         (read (buffer-string)))))
;;             (str)))

(defconst ex-buffer-name "*example*")

;; (push '("*example*" :position bottom) popwin:special-display-config)
;; (ex-put-example )
(defun ex-example (func)
  (interactive)
  (ex-kill-ex-buffer)
  (when (fboundp (eval 'func))
    (let ((buf (get-buffer-create ex-buffer-name)))
      (progn
        (get-buffer buf)
        (pop-to-buffer buf)
        (insert (format "%s\n" (symbol-name (eval 'func))))
        (insert (format "%s\n" (gethash (symbol-name (eval 'func)) ex-hash))))
        )
    (insert "not bounded")))
;; (defun ex-example )
;; (eval-string (ex-get-example 'example))
;; (ex-example 'ex-foo)


(defun ex-delete-window ()
  (interactive)
  (let ((win (get-buffer-window ex-buffer-name)))
    (delete-window win)))

(defvar ex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'ex-delete-window)
    map))

(define-derived-mode ex-mode nil "Example"
  ""
  (setq buffer-read-only t)
  (use-local-map ex-mode-map))

(defun ex-kill-ex-buffer ()
  (if (get-buffer ex-buffer-name)
      (kill-buffer ex-buffer-name)))
