;;; r-like-example-test.el --- r-like-example-test
;;; Commentary:
;;; Code:
(require 'ert)
(require 'r-like-example)
(require 'elisp-examples)

;; (add-to-list 'popwin:special-display-config '("*example*" :position right :width 50 :stick t))

(defun ex-test-set-env ()
  "Set r-like-example env."
  (defconst test-example-foo '("(message \"t\")"))

  (defconst test-example-bar '("(defun __ex-bar (bool) (if bool t nil))"
                           "(__ex-bar t)"
                           "(__ex-bar nil)"))

  (ex-put-example 'ex-foo test-example-foo)
  (ex-put-example 'ex-bar test-example-bar)
  )

(ex-test-set-env)

(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))

(ert-deftest ex-get-example-test ()
  (should (equal '("(message \"t\")") (ex-get-example 'ex-foo))))

(ert-deftest ex-put-example-test ()
  (should (equal '("(message \"t\")") (ex-put-example 'ex-foo test-example-foo))))

(ert-deftest ex-key-exists-p-test ()
  (should (equal t (ex-key-exists-p 'setq t)))
  (should (equal "setq: exist" (ex-key-exists-p 'setq))))

(ert-deftest ex-example-test ()
  (should (equal (mapconcat 'identity
                            (list (car test-example-foo)
                                  (concat ex-begin-comment "\"t\"")
                                  ex-separator)
                            "\n")
                 (progn
                   (ex-example 'ex-foo)
                   (buffer-string)
                   ))))


;; ;; dump example
;; (maphash #'(lambda (key val)
;;              (ex-example (intern-soft key)))
;;          ex-hash)

;;; r-like-example-test.el ends here
