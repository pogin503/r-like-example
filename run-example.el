;; (puthash "example" "(defun ex-foo () nil) (example 'ex-foo)" ex-hash)
;; (puthash "example" '("(defun ex-foo () nil) (example 'ex-foo)") ex-hash)

(require 'r-like-example)
;; (add-to-list 'load-path "~/repo/r-like-example")
;; (define-key global-map (kbd "s-9") 'ex-example)
;; (define-key global-map (kbd "M-9") 'ex-example)
;; (define-key global-map (kbd "s-0") 'ex-insert-current-buffer)
;; (define-key global-map (kbd "C-c 0") 'ex-add-example)
;; (global-set-key (kbd "C-c 9") 'ex-delete-last-elem)

(defun __ex-foo () (message "t" ))
(setq test-example-foo '("(__ex-foo)"))

(defun __ex-bar (bool) (if bool t nil))
(setq test-example-bar '("(__ex-bar t)"
                      "(__ex-bar nil)"))

(ex-put-example '__ex-foo test-example-foo)
(ex-put-example '__ex-bar test-example-bar)
(ex-get-example '__ex-foo)
(ex-get-example '__ex-bar)
(add-to-list 'popwin:special-display-config '("*example*" :position right :width 50))
;; (ex-put-example '__ex-foo '("(__ex-foo)"))
;; (ex-get-example '__ex-foo)
(ex-example '__ex-foo)
(mapcar #'(lambda (key)
           (ex-example key)) '(__ex-foo __ex-bar))

(setq example-mul '("(__ex-foo)" "(__ex-bar t)" "(__ex-bar nil)"))
;; (mapcar #'ex-example example-mul)
;; (ex-examples '(__ex-foo __ex-bar))
