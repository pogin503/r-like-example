;; (puthash "example" "(defun ex-foo () nil) (example 'ex-foo)" ex-hash)
;; (puthash "example" '("(defun ex-foo () nil) (example 'ex-foo)") ex-hash)


(ex-put-example '__ex-foo "(defun __ex-foo () nil)  (example '__ex-foo)")

(ex-get-example '__ex-foo)
(mapcar (lambda (x) (eval-string x)) (ex-get-example '__ex-foo))
