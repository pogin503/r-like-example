;;; r-like-example-test --- r-like-example-test
;;; Commentary:
;;; Code:
(require 'ert)
(require 'r-like-example)
(require 'elisp-examples)

;; (add-to-list 'popwin:special-display-config '("*example*" :position right :width 50))

(defun ex-test-set-env ()
  "Set r-like-example env."
  (defconst test-example-foo '("(message \"t\" ))"))

  (defconst test-example-bar '("(defun __ex-bar (bool) (if bool t nil))"
                           "(__ex-bar t)"
                           "(__ex-bar nil)"))

  (ex-put-example 'ex-foo test-example-foo)
  )

(ex-test-set-env)

(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))

(ert-deftest ex-put-example-test ()
  (ex-test-set-env)
  (message (car test-example-foo))
  (should (equal '("(message \"t\" ))") (ex-put-example 'ex-foo test-example-foo))))

(ert-deftest ex-get-example-test ()
  (ex-test-set-env)
  (should (equal test-example-foo (ex-get-example 'ex-foo))))
(ert-deftest ex-key-exists-p-test ()
  (should (equal t (ex-key-exists-p 'setq t)))
  (should (equal "setq: exist" (ex-key-exists-p 'setq))))

(ert-deftest ex-example-test ()
  (should (equal (mapconcat 'identity `(,(car test-example-foo) ";=> \"t\""  ,ex-separator) "\n")
                 (progn
                   (let (buf (current-buffer))
                     (save-excursion
                       (get-buffer-create ex-buffer-name)
                       (switch-to-buffer ex-buffer-name)
                       (erase-buffer)
                       (switch-to-buffer buf)
                       (ex-example 'ex-foo)
                       (switch-to-buffer ex-buffer-name)
                       (setq res (buffer-substring-no-properties (point-min) (point-max)))
                       (switch-to-buffer buf))
                     res)))))


;; ;; dump example
;; (maphash #'(lambda (key val)
;;              (ex-example (intern-soft key)))
;;          ex-hash)

;;; r-like-example-test ends here
