(require 'r-like-example)
;; (add-to-list 'load-path "~/repo/r-like-example")
(setq test-example-foo '("(defun __ex-foo () (message \"t\" ))" "(__ex-foo)"))

(setq test-example-bar '("(defun __ex-bar (bool) (if bool t nil))"
                      "(__ex-bar t)"
                      "(__ex-bar nil)"))

(ex-put-example '__ex-foo test-example-foo)
(ex-put-example '__ex-bar test-example-bar)
;; (add-to-list 'popwin:special-display-config '("*example*" :position right :width 50))
;; (pop popwin:special-display-config)
(ex-put-example '__ex-foo '("(defun __ex-foo () (message \"t\" ))" "(__ex-foo)"))
(ex-get-example '__ex-foo-test)
(ex-example '__ex-foo)
(mapcar #'(lambda (x)
           (ex-example x)
           ) '(__ex-foo __ex-bar))

;; dump example
(maphash #'(lambda (key val)
             (ex-example (intern-soft key)))
         ex-hash)
