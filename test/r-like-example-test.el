;;; r-like-example-test.el --- r-like-example-test
;;; Commentary:
;;; Code:
(require 'ert)
(require 'r-like-example)
(load "./examples/elisp-examples.el")

(defun ex-test-set-env ()
  "Set r-like-example env."
  (defconst test-example-foo '("(message \"t\")"))

  (defconst test-example-bar '("(defun __ex-bar (bool) (if bool t nil))"
                           "(__ex-bar t)"
                           "(__ex-bar nil)"))

  (ex-put-example 'ex-foo test-example-foo)
  (ex-put-example 'ex-bar test-example-bar))

(ex-test-set-env)

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

(ert-deftest ex-insert-current-buffer-test ()
  (should (equal (with-temp-buffer
                   (ex-get-example 'ex-foo)
                   (ex-insert-current-buffer 'ex-foo)
                   (buffer-string))
                 "(ex-put-example 'ex-foo '(\"(message \\\"t\\\")\"))"
                 )))

(ert-deftest ex-add-example-test ()
  (should (equal (with-temp-buffer
                   (insert "(setq x 123)")
                   (ex-add-example))
                 "\"(setq x 123)\""))
  (should (equal (with-temp-buffer
                   (insert "(setq x 123)")
                   (goto-char (point-min))
                   (ex-add-example))
                 "\"(setq x 123)\""))
  (should-error (with-temp-buffer
                  (insert "\n(setq x 123)")
                  (goto-char (point-min))
                  (ex-add-example)))
  )

;; ;; dump example
;; (maphash #'(lambda (key val)
;;              (ex-example (intern-soft key)))
;;          ex-hash)

;;; r-like-example-test.el ends here
