;; (puthash "example" "(defun ex-foo () nil) (example 'ex-foo)" ex-hash)
;; (puthash "example" '("(defun ex-foo () nil) (example 'ex-foo)") ex-hash)

(require 'r-like-example)
;; (add-to-list 'load-path "~/repo/r-like-example")
(defun __ex-foo () (message "t" ))
(setq test-example-foo '("(__ex-foo)"))

(defun __ex-bar (bool) (if bool t nil))
(setq test-example-bar '("(__ex-bar t)"
                      "(__ex-bar nil)"))

(ex-put-example '__ex-foo test-example-foo)
(ex-put-example '__ex-bar test-example-bar)
(ex-get-example '__ex-foo)
(ex-get-example '__ex-bar)
;; (push '("*example*" :position bottom) popwin:special-display-config)
(add-to-list 'popwin:special-display-config '("*example*" :position bottom))
;; (ex-put-example '__ex-foo '("(__ex-foo)"))
;; (ex-get-example '__ex-foo)
(ex-example '__ex-foo)
(mapcar #'(lambda (x)
           (ex-example x)) '(__ex-foo __ex-bar))

(setq example-mul '(__ex-foo __ex-bar))
(mapcar #'ex-example example-mul)
(ex-examples '(__ex-foo __ex-bar))
