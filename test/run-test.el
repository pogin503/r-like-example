;;; run-test --- run-test
;;; Commentary:

;;; Usage:
;;    emacs -Q -l tests/run-test.el # interactive mode
;;    emacs -batch -Q -l tests/run-test.el # batch mode
;;  @see https://github.com/uk-ar/key-combo/blob/master/test/run-test.el
;;  @see http://d.hatena.ne.jp/yuheiomori0718/20111230/1325246364
;;  @see http://sakito.jp/emacs/emacsshell.html#path

;;; Code:

(message "Running tests on Emacs %s" emacs-version)

;; Utils
(defun ex-test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'ex-test-join-path rest))
    path))

(defconst ex-test-dir
  (if load-file-name
      (file-name-directory load-file-name)
    ;; Fall back to default directory (in case of M-x eval-buffer)
    default-directory)
  "Directory of the test suite.")

(defconst ex-root-dir (expand-file-name (concat ex-test-dir "..")))

;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list ex-test-dir
            ex-root-dir))

(load "r-like-example-test")

;; (require 'cask "~/.cask/cask.el")
;; (cask-initialize)

;; Run tests
(when noninteractive
    (ert-run-tests-batch-and-exit))

;;; run-test ends here
