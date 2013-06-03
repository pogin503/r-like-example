;; Usage:
;;
;; emacs -Q -l tests/run-test.el # interactive mode
;; emacs -batch -Q -l tests/run-test.el # batch mode

;; @see https://github.com/uk-ar/key-combo/blob/master/test/run-test.el

;; Utils
(defun r-like-example-test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'r-like-example-test-join-path rest))
    path))

(defvar r-like-example-test-dir (file-name-directory load-file-name))
(defvar r-like-example-root-dir (concat r-like-example-test-dir ".."))


;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list r-like-example-test-dir
            r-like-example-root-dir))


;; Use ERT from github when this Emacs does not have it
(unless (locate-library "ert")
  (add-to-list
   'load-path
   (r-like-example-test-join-path r-like-example-root-dir "ert")))

;; ;; Use el-spec
;; (add-to-list
;;  'load-path
;;  (r-like-example-test-join-path r-like-example-root-dir "lib" "el-spec"))

;; ;; el-mock
;; (add-to-list
;;  'load-path
;;  (r-like-example-test-join-path r-like-example-root-dir "lib" "el-mock"))

;; Load tests
(load "r-like-example-test")


;; Run tests
(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))
