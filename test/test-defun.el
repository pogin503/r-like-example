;; test-defun.el

(setq ex-test-hs (make-hash-table :test #'equal))

(defun ex-put-example-test (symbol example &optional (hash 'ex-hash))
  (puthash (symbol-name (eval 'symbol)) example hash)
  )
(defun ex-get-example-test (symbol &optional '(hash 'ex-hash))
  (gethash (symbol-name (eval 'symbol)) hash))

(ex-get-example-test 'car 'ex-test-hs)

(defun ex-example-test (symbol &optional '(hash 'ex-hash))
  (interactive "aSymbol name? ")
  ;; (ex-kill-ex-buffer)
  (when (if (stringp symbol)
            (read sym)
          (fboundp (eval 'symbol)))
      ;; (intern-soft symbol)
    (let ((buf (get-buffer-create ex-buffer-name)))
      (get-buffer buf)
      (pop-to-buffer buf)
      (lisp-interaction-mode)
      (goto-char (point-min))
      (ex-insert-example-test symbol hash)
      ;; (insert sep))
    )))
(defun ex-insert-example-test (symbol &optional (hash 'ex-hash))
  (goto-char (point-min))
  (mapcar #'(lambda (ex)
              ;; (when (fboundp (eval 'symbol))
              (insert (format "%s\n" ex))
              (insert ";=> ")
              (save-excursion
                (let ((ex1 (ex-eval-string ex)))
                  (if (stringp ex1)
                      (insert (format "\"%s\"\n" ex1))
                    (insert (format "%s\n" ex1))))
                )
              (forward-line 1)
              ) (ex-get-example-test (eval 'symbol) hash))
  (insert ex-separator)
  )

(provide 'test-defun)

;;; end test-defun
