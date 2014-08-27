;;; test-helper.el --- test-helper.el
;;; Commentary:

;;; Usage:
;;    emacs -Q -l tests/test-helper.el # interactive mode
;;    emacs -batch -Q -l tests/test-helper.el # batch mode
;;  @see https://github.com/uk-ar/key-combo/blob/master/test/run-test.el
;;  @see http://d.hatena.ne.jp/yuheiomori0718/20111230/1325246364
;;  @see http://sakito.jp/emacs/emacsshell.html#path

;;; Code:

;; Utils
(defun ex-test/join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'ex-test/join-path rest))
    path))

(defconst ex-test/test-path
  (if load-file-name
      (file-name-directory load-file-name)
    ;; Fall back to default directory (in case of M-x eval-buffer)
    default-directory)
  "Directory of the test suite.")

(defconst ex-test/root-path
  (directory-file-name (file-name-directory (concat ex-test/test-path "../"))))

;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list ex-test/test-path
            ex-test/root-path))

(load (expand-file-name "r-like-example" ex-test/root-path) 'noerror 'nomessage)

;; Run tests
;; (when noninteractive
;;     (ert-run-tests-batch-and-exit))

;;; test-helper.el ends here
