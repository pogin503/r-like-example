;;; r-like-example.el --- r-like-example

;; Copyright (C) 2012

;; Author:  <pogin>
;; Keywords: lisp


(defconst ex-hash (make-hash-table :test #'equal)
  "store example")
;; ex-hash

(defun ex-put-example (symbol example)
  (puthash (symbol-name (eval 'symbol)) example ex-hash)
  )

(defun ex-get-example (symbol)
  (gethash (symbol-name (eval 'symbol)) ex-hash))

(defun __ex-foo () t)

(defun eval-string (str)
  (eval (with-temp-buffer
          (insert str)
          (read (buffer-string)))))


(defconst ex-buffer-name "*example*")

(defun ex-example (symbol)
  (interactive)
  ;; (ex-kill-ex-buffer)
  (when (fboundp (eval 'symbol))
    (let ((buf (get-buffer-create ex-buffer-name))
          (sep "=================================\n"))
      (get-buffer buf)
      (pop-to-buffer buf)
      (goto-char (point-min))
      ;; (mapcar (lambda (x)
      ;;           (insert (format "%s\n" x))
      ;;           ;; (insert (eval-string x))
      ;;           (insert ";=> ")
      ;;           ;; (type-of (eval-string x))
      ;;           (insert (format "%s\n" (eval-string x)))
      ;;           ) (ex-get-example (eval 'symbol)))
      (ex-insert-example symbol)
      ;; (insert sep))
    )))

(defun ex-examples (symbols)
  (interactive)
  (let ((buf (get-buffer-create ex-buffer-name))
        (sep "=================================\n")
        )
    (get-buffer buf)
    (pop-to-buffer buf)
    (mapcar
     ;; #'(lambda (symbol)
     ;;       (goto-char (point-min))
     ;;       (mapcar #'(lambda (ex)
     ;;                   ;; (when (fboundp (eval 'symbol))
     ;;                   (insert (format "%s\n" ex))
     ;;                   (insert ";=> ")
     ;;                   (insert (format "%s\n" (eval-string ex)))
     ;;                   ) (ex-get-example (eval 'symbol))))
     #'ex-insert-example
     symbols)))

(defun ex-insert-example (symbol)
  (goto-char (point-min))
  (mapcar #'(lambda (ex)
              ;; (when (fboundp (eval 'symbol))
              (insert (format "%s\n" ex))
              (insert ";=> ")
              (let ((ex1 (eval-string ex)))
                (if (equal (type-of ex1) 'string)
                    (insert (format "\"%s\"\n" ex1))
                  (insert (format "%s\n" ex1))))

              ) (ex-get-example (eval 'symbol)))
  (insert "=================================\n")
  )

;; (eval-string (ex-get-example 'example))
;; (ex-example '__ex-foo)


(defun ex-delete-window ()
  (interactive)
  (let ((win (get-buffer-window ex-buffer-name)))
    (delete-window win)))

(defvar ex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'ex-delete-window)
    map))

(define-derived-mode ex-mode nil "Example"
  ""
  (setq buffer-read-only t)
  (use-local-map ex-mode-map))

(defun ex-kill-ex-buffer ()
  (if (get-buffer ex-buffer-name)
      (kill-buffer ex-buffer-name)))

(provide 'r-like-example)
