;;; r-like-example.el --- r-like-example

;; Copyright (C) 2012

;; Author:  <pogin>
;; Keywords: lisp


(defcustom ex-debug t
  "Enable debug config"
  :type 'boolean
  :group 'ex)

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

;; (defun ex-eval-string (str)
;;   (with-temp-buffer
;;     (get-buffer-create "*example*")
;;     (mapcar #'(lambda (x)
;;                 (eval (with-temp-buffer
;;                         (insert str)
;;                         (read (buffer-string)))))
;;             (str)))

(defconst ex-buffer-name "*example*")

(defun ex-example (symbol)
  (interactive)
  (ex-kill-ex-buffer)
  (when (fboundp (eval 'symbol))
    (let ((buf (get-buffer-create ex-buffer-name)))
      (progn
        (get-buffer buf)
        (pop-to-buffer buf)
        ;; (insert (format "%s\n" (symbol-name (eval 'symbol))))
        (insert (format "%s\n" (gethash (symbol-name (eval 'symbol)) ex-hash)))
        (insert (format "%s => %s" (ex-get-example 'symbol) )))
        ))
    (insert "end example"))
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

(dont-compile
  (when ex-debug
    (progn
      (defun __ex-foo ()
        t)
      (ex-put-example '__ex-foo "(defun __ex-foo () t) (__ex-foo)")
      (ex-put-example '__ex-foo '("(defun __ex-bar (bool) (if bool t nil)) (__ex-bar t)"
                               "(__ex-bar nil)"))
      (push '("*example*" :position bottom) popwin:special-display-config)
      )
    ))
