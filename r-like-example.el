;;; r-like-example.el --- r-like-example

;; Copyright (C) 2012

;; Author:  <pogin>
;; Keywords: lisp

(defconst ex-hash (make-hash-table :test #'equal)
  "hash"
  )
(puthash "example" "(defun ex-test () nil) (example 'ex-test)" ex-hash)
;; (puthash "example" '("(defun ex-test () nil)"  "(example 'ex-test)") ex-hash)

(defun ex-put-example (symbol example)
  (puthash (symbol-name (eval 'symbol)) example ex-hash)
  )
;; (put-example 'example "(defun ex-test () nil)  (example 'ex-test)")

(defun ex-get-example (symbol)
  (gethash (symbol-name (eval 'symbol)) ex-hash))
;; (get-example 'example)


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

(defun ex-example (func)
  (when (fboundp (eval 'func))
      (progn
        (insert (format "%s" (symbol-name (eval 'func))))
        (insert (format "%s" (gethash (symbol-name (eval 'func)) ex-hash))))
        ;; (set-buffer (get-buffer-create "*example*"))
        )
    (insert "not bounded"))

;; (eval-string (get-example 'example))
