;;; r-like-example-test.el --- r-like-example-test
;;; Commentary:
;;; Code:
(require 'ert)
(require 'r-like-example)
(load "./examples/elisp-examples.el")

(defun ex-test-set-env ()
  "Set r-like-example env."
  (defconst test-example-foo "(defun ex-foo () (message \"t\"))")
  (defconst test-example-bar '("(defun __ex-bar (bool) (if bool t nil))"
                           "(__ex-bar t)"
                           "(__ex-bar nil)"))

  (ex-put-example 'ex-foo test-example-foo t)
  (ex-set-example 'ex-bar '())
  (mapc (lambda (x) (ex-put-example 'ex-bar x)) test-example-bar))

(ex-test-set-env)

(ert-deftest ex-get-example-test ()
  (ex-test-set-env)
  (should (equal nil (ex-get-example 'nil-key)))
  (should (equal `(,test-example-foo) (ex-get-example 'ex-foo))))

(ert-deftest ex-put-example-test ()
  (ex-test-set-env)
  (should (equal `(,test-example-foo) (ex-put-example 'ex-foo test-example-foo t)))
  (should (equal `(,test-example-foo) (ex-put-example 'ex-foo `(,test-example-foo) t)))
  (ex-put-example 'ex-foo '() t)
  (should (equal `(,test-example-foo) (ex-put-example 'ex-foo test-example-foo)))
  (should (equal `(,test-example-foo ,test-example-foo) (ex-put-example 'ex-foo test-example-foo)))
  (ex-put-example 'ex-foo '() t)
  (should (equal `(,test-example-foo) (ex-put-example 'ex-foo `(,test-example-foo)))))

(ert-deftest ex-key-exists-p-test ()
  (should (equal t (ex-hash-key-exists-p "setq" ex-hash)))
  (should (equal "setq: exist" (ex-query-key-exists 'setq))))

(ert-deftest ex-example-test ()
  (should (equal (mapconcat 'identity
                            (list
                             ";; ex-foo examples"
                             ";; ex-foo: ()"
                             test-example-foo
                             (concat ex-begin-comment "ex-foo")
                             ex-separator)
                            "\n")
                 (progn
                   (with-current-buffer (get-buffer-create ex-buffer-name)
                     (erase-buffer))
                   (ex-example 'ex-foo)
                   (buffer-string))))
  (should (equal (mapconcat 'identity
                            (list
                             ";; ex-foo examples"
                             ";; ex-foo: ()"
                             test-example-foo
                             (concat ex-begin-comment "ex-foo")
                             ex-separator)
                            "\n")
                 (progn
                   (with-current-buffer (get-buffer-create ex-buffer-name)
                     (erase-buffer))
                   (ex-example 'ex-foo)
                   (buffer-string)))))

(ert-deftest ex-insert-current-buffer-test ()
  (ex-test-set-env)
  (should (equal (with-temp-buffer
                   (ex-get-example 'ex-foo)
                   (ex-insert-current-buffer 'ex-foo)
                   (buffer-string))
                 "(ex-put-example 'ex-foo '(\"(defun ex-foo () (message \\\"t\\\"))\") t)"
                 )))

(ert-deftest ex-add-example-test ()
  (ex-test-set-env)
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
