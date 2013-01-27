;; elisp-examples.el

(require 'r-like-example)

;; base
(ex-put-example 'setq '("(setq x 3)"
  "(setq foo 'apple)"
  "foo"
))

;; array
(ex-put-example 'arrayp '(
  "(setq __abar '(1 2))"
  "(setq __x (vector '__foo __abar))"
  "(arrayp __x)"
))

(ex-put-example 'copy-sequence
'("(setq __abar '(1 2))"
  "(setq __x (vector '__foo __abar))"
  "(setq __y (copy-sequence __x))"
))

(ex-put-example 'aref '(
  "(setq __primes [2 3 5 7 11])"
  "(aref __primes 4)"
  "(aref \"abcdefg\" 1)"
))

(ex-put-example 'aset '(
  "(setq __w [__foo __bar __baz])"
  "(aset __w 0 '__fu)"
  "__w"
  "(setq __x \"asdfasfd\")"
  "(aset __x 3 ?Z)"
  "__x"
))

(ex-put-example 'fillarray '(
  "(setq a [a b c d e f g])"
  "(fillarray a 0)"
  "a"
  "(setq s \"When in the course\")"
  "(fillarray s ?-)"
))

(ex-put-example 'elt '(
  "(elt [1 2 3 4] 2)"
  "(elt '(1 2 3 4) 2)"
  "(string (elt \"1234\" 2))"
;; (elt [1 2 3 4] 4)
;; (elt [1 2 3 4] -1)
))

(ex-put-example 'length '(
  "(length '(1 2 3))"
  "(length ())"
  "(length \"foobar\")"
  "(length [1 2 3])"
  "(length (make-bool-vector 5 nil))"
))

(ex-put-example 'vectorp '(
  "(vectorp [a])"
  "(vectorp \"asdf\")"
))

(ex-put-example 'vector '(
  "(vector 'foo 23 [bar baz] \"rats\")"
  "(vector)"
))

(ex-put-example 'make-vector '(
  "(setq sleepy (make-vector 9 'Z))"
))

(ex-put-example 'vconcat '(
  "(setq a (vconcat '(A B C) '(D E F)))"
  "(eq a (vconcat a))"
  "(vconcat)"
  "(vconcat [A B C] \"aa\" '(foo (6 7)))"
))

;; list function
(ex-put-example 'car '("(car '(1 2 3))" "(car '(a b c))"))
(ex-put-example 'cdr '("(cdr '(1 2 3))" "(cdr '(a b c))"))
(ex-put-example 'setcar '("(setq animals '(antelope giraffe lion tiger))"
  "animals"
  "(setcar animals 'hippopotamus)"
  "animals"))

(ex-put-example 'cons '("(cons 'buttercup ())"
  "(cons 'daisy '(buttercup))"
  "(cons 'violet '(daisy buttercup))"
  "(cons 'rose '(violet daisy buttercup))"))

(ex-put-example 'first '("(first '(1 2 3))" "(first '(a b c))"))
(ex-put-example 'rest '("(rest '(1 2 3))" "(rest '(a b c))"))

(ex-put-example 'nthcar  'nil)
(ex-put-example 'nthcdr  'nil)
(ex-put-example 'nthcdr+  'nil)

;; Numbers
(ex-put-example 'logb '("(logb 10)"
  "(logb 10.0e20)"))

(ex-put-example 'max '("(max 1 3 5 4 2)"
  "(max 20)"
  "(max 1 2.5)"
  "(max 1 3 2.5)"))

(ex-put-example 'zerop '("(setq x 0)"
  "(zerop x)"))

(ex-put-example 'min '("(min -4 1)"
  "(min 2 4.5 9)"))

(ex-put-example 'abs '("(abs -10)"))


(ex-put-example 'truncate '("(truncate 1.2)"
  "(truncate 1.7)"
  "(truncate -1.2)"
  "(truncate -1.7)"))
(ex-put-example 'floor '("(floor 1.2)"
  "(floor 1.7)"
  "(floor -1.2)"
  "(floor -1.7)"
  "(floor 5.99 3)"))

(ex-put-example 'ceiling '("(ceiling 1.2)"
  "(ceiling 1.7)"
  "(ceiling -1.2)"
  "(ceiling -1.7)"))

(ex-put-example 'round '("(round -1.7)"
  "(round -1.2)"
  "(round 1.7)"
  "(round 1.2)"))

(ex-put-example '+ '("(+)"
  "(+ 1)"
  "(+ 1 2 3 4)"))
(ex-put-example '- '("(- 10 1 2 3 4)"
  "(- 10)"
  "(-)"))
(ex-put-example '* '("(*)"
  "(* 1)"
  "(* 1 2 3 4)"))
(ex-put-example '/ '("(/ 6 2)"
  "(/ 5 2)"
  "(/ 5.0 2)"
  "(/ 5 2.0)"
  "(/ 5.0 2.0)"
  "(/ 25 3 2)"
  "(/ -17 6)"
  "(/ 6 2)"))
(ex-put-example '% '("(% 9 4)"
  "(% -9 4)"
  "(% 9 -4)"
  "(% -9 -4)"))
(ex-put-example 'mod '("(mod 9 4)"
  "(mod -9 4)"
  "(mod 9 -4)"
  "(mod -9 -4)"
  "(mod 5.5 2.5)"))

;; higher-order function
(ex-put-example 'funcall
                '(";; (funcall function arg1 arg2 ...)
(funcall #'car '(a b c))"
  "(funcall #'car '(1 2 3))"
  "(funcall #'+ 1 2 3)"
  "(funcall 'position 1 '(1 2 3 2 1) :start 1)"
  "(defun __execfunc (func arg1 arg2)
  (funcall func arg1 arg2))"
  "(__execfunc #'+ 1 2)"
))

(ex-put-example 'apply '(
  "(apply #'car '((a b c)))"
  "(apply #'list '(1 2 3))"
;; "(apply #'+ 1 2)"
  "(apply #'max '(1 5 10 3 4))"
  "(apply #'+ 4 5 6 '(1 2 3))"
  "(apply #'+ (mapcar #'length '(\"abc\" \"defg\" \"hijkl\" \"mnopqr\")))"
))

(ex-put-example 'mapcar
                '(
                  ;; "(mapcar #'* '(1 2 3 4 5) '(10 20 30 40 50))"
                  "(mapcar #'car '((a . 1) (b . 2) (c . 3)))"
                  "(mapcar #'cdr '((a . 1) (b . 2) (c . 3)))"
                  "(mapcar #'(lambda (x) (* x x)) '(1 2 3 4 5))"
                  ;; "(mapcar #'cons '(a b c d) '(1 2 3 4 5))"
                  ))
(ex-put-example 'mapcar* '("(mapcar* 'cons '(a b c) '(1 2 3 4))"))
(ex-put-example 'mapconcat '("(mapconcat 'symbol-name '(The cat in the hat) \" \")"
                             "(mapconcat 'identity '(\"\" \"home\" \"alex\" \"elisp\" \"erc\") \"/\")"))
(ex-put-example 'mapcan '(";; (mapcan FUNCTION SEQUENCE...)
;; (mapcan f x1 ... xn) == (apply #'nconc (mapcar f x1 ... xn))
(mapcan #'(lambda (x y) (if (null x) nil (list x y)))
         '(nil nil nil d e)
         '(1 2 3 4 5 6))"))

;; (ex-put-example 'map '(""))

(ex-put-example 'mapc '("(mapc #'(lambda (i) i) '(1 2 3))"))
(ex-put-example 'mapl    '(";; function &rest lists+ => list-1
(mapl #'(lambda (x y) (print (append x y))) (list 1 0 2) (list 3 4 5))"
                           ))
(ex-put-example 'maplist '(";; function &rest lists+ => result-list
(maplist #'list (list 1 2 3) (list 4 5 6))"
                           ))
(ex-put-example 'mapcon  '(";; function &rest lists+ => concatenated-results
(mapcon #'list (list 1 2 3) (list 4 5 6))"
                           ))

;; for obarray
(ex-put-example 'mapatoms '("(setq __count 0)"
  "(defun __count-syms (s)
  (setq __count (1+ __count)))"
  ";; (mapatoms FUNCTION &optional OBARRAY)
(mapatoms '__count-syms)"
  "__count")
)

(ex-put-example 'member '("(member 3 '(1 2 3 4 5))"
                          "(member 30 '(1 2 3 4 5))"))
(ex-put-example 'find '("(find 3 '(1 2 3 4 5))"))
(ex-put-example 'max '("(max 1 2 3 4 5)"))
(ex-put-example 'sort '("(sort '(1 -2 3 4 -5) '<)"))
(ex-put-example 'sort* '("(sort* '(1 -2 3 4 -5) '< :key 'abs)"))
(ex-put-example 'equal '(
                         "(equal 'foo 'foo)"
                         "(equal 456 456)"
                         "(equal \"asdf\" \"asdf\")"
                         "(eq \"asdf\" \"asdf\")"
                         "(equal '(1 (2 (3))) '(1 (2 (3))))"
                         "(eq '(1 (2 (3))) '(1 (2 (3))))"
                         "(equal [(1 2) 3] [(1 2) 3])"
                         "(eq [(1 2) 3] [(1 2) 3])"
                         "(equal (point-marker) (point-marker))"
                         "(eq (point-marker) (point-marker))"
                         ))

(ex-put-example 'eq '(
                      "(equal 'foo 'foo)"
                      "(equal 456 456)"
                      "(equal \"asdf\" \"asdf\")"
                      "(equal '(1 (2 (3))) '(1 (2 (3))))"
                      "(eq '(1 (2 (3))) '(1 (2 (3))))"
                      "(equal [(1 2) 3] [(1 2) 3])"
                      "(eq [(1 2) 3] [(1 2) 3])"
                      "(equal (point-marker) (point-marker))"
                      "(eq (point-marker) (point-marker))"
                      ))

(ex-put-example 'print '("(print \"Hello World\")"))
(ex-put-example 'prin1 '("(prin1 \"Hello World\")"))
(ex-put-example 'princ '("(princ \"Hello World\")"))

(ex-put-example 'format '("(format \"%d\" 123)"))
(ex-put-example 'makunbound '("(setq __x 1)" "__x" "(makunbound '__x)" "__x"))
(ex-put-example 'fmakunbound '("(defun __func () (format \"__func\"))"
                              "(__func)"
                              "(fmakunbound '__func)"
                              "(__func)"))

;; object composition
(ex-put-example 'defstruct '(
                         "(require 'cl)"
                         "(defstruct person name age)"
                         "(setq foo (make-person :name \"foo\" :age 16))"
                         "(person-p foo) "
                         "(person-name foo)"
                         "(person-age foo)"))

;; hash table
(ex-put-example 'make-hash-table
                '("(setq __hash_table (make-hash-table :test #'equal))"))
(ex-put-example 'remhash '("(setq __hash_table (make-hash-table :test #'equal))"
                           "(puthash \"apple\" 150 __hash_table)"
                           "(gethash \"apple\" __hash_table)"
                           "(hash-table-count __hash_table)"
                           "(remhash \"apple\" __hash_table)"
                           "(hash-table-count __hash_table)"))
(ex-put-example 'gethash '("(setq __hash_table (make-hash-table :test #'equal))"
                           "(puthash \"apple\" 150 __hash_table)"
                           "(gethash \"apple\" __hash_table)"))
(ex-put-example 'puthash '("(setq __hash_table (make-hash-table :test #'equal))"
                           "(puthash \"apple\" 150 __hash_table)"
                           "(puthash \"orange\" 300 __hash_table)"
                           "(puthash \"banana\" 100 __hash_table)"
                           ))
(ex-put-example 'maphash '(
  "(setq __hash_table (make-hash-table :test #'equal))"
  "(puthash \"apple\" 150 __hash_table)"
  "(puthash \"orange\" 300 __hash_table)"
  "(puthash \"banana\" 100 __hash_table)"
  "(maphash #'(lambda (key value)
             (insert (format \"key=>%S,value=>%S\n\" key value))) __hash_table)"))

;; association list
(ex-put-example 'assoc '("(setq __alist '((\"rose\" . red) (\"violet\" . blue)))"
                         "(assoc \"rose\" __alist)"))
(ex-put-example 'assq '("(setq __alist '((red . \"rose\") (blue . \"violet\")))"
                         "(assq 'red __alist)"))
;; (ex-put-example 'assoc-string '("(setq __alist '((\"rose\" . red) (\"violet\" . blue)))"
;;                                 "(assoc \"rose\" __alist)"))
(ex-put-example 'assoc '("(setq __alist '((\"rose\" . red) (\"violet\" . blue)))"
                         "(assoc \"rose\" __alist)"))


;; string
(ex-put-example 'split-string '("(split-string \"aaa,bbb,ccc\" \",\")"))
(ex-put-example 'concat '("(concat \"aaa\" \"bbb\")"))
(ex-put-example 'length '("(length \"abcde\")"
                          "(length \"\あいうえお\")"))
(ex-put-example 'string-width '("(string-width \"あいうえお\")"))
(ex-put-example 'substring '("(substring \"abcdef\" 0 2)"))
(ex-put-example 'string-match '("(string-match \"bc\" \"abcde\")"))
(ex-put-example 'match-string '(
;; ";; When "
  "(let ((str \"apple 12345\"))
  (when (string-match \"\\\\([0-9]+\\\\)\" str)
    (match-string 1 str)))"
))

;; loop
(ex-put-example 'dolist '(
  "(dolist (i '(1 2 3))
  (insert (format \"%d \" i)))"))

(ex-put-example 'dotimes '(
  "(dotimes (i 5)
  (insert (format \"%d \" i)))"))

(ex-put-example 'do '(
  "(dotimes (i 5)
  (insert (format \"%d \" i)))"))

(ex-put-example 'loop '("(loop for x in '(1 2 3 4 5)
      do (insert (format \"%d \" (* x x))))"
  "(loop for x in '(1 2 3 4 5)
      for y in '(6 7 8 9 10)
      collect (list x y))"
  "(loop for x from 1 to 5
      for y = (* x 2)
      collect y)"
  "(loop for x in '(foo 2)
      thereis (numberp x))"
  "(loop for x from 1 to 10
      collect (loop for y from 1 to x
                    collect y))"
  "(loop for i upto 10 collect i)"
  "(loop for i from 0 downto -10 collect i)"
  "(loop for i in '(10 20 30 40) by #'cddr collect i)"
  "(loop for x across \"abcd\" collect x)"
  "(loop repeat 5
      for x = 0 then y
      for y = 1 then (+ x y)
      collect y)"))


(ex-put-example 'remove-if-not '("(remove-if-not #'evenp '(1 2 3 4 5))"))

;; (ex-put-example 'nth  'nil)
;; (ex-put-example 'nth-value  'nil)
;; (ex-put-example 'ninth  'nil)
;; (ex-put-example 'nintersection  'nil)
;; (ex-put-example 'nil-1  'nil)
;; (ex-put-example 'nil-2  'nil)
;; (ex-put-example 'nil-blank-string  'nil)
;; (ex-put-example 'append  'nil)
;; (ex-put-example 'append-to-list  'nil)
;; (ex-put-example 'append-to-file  'nil)
;; (ex-put-example 'append-to-buffer  'nil)
;; (ex-put-example 'point-at-bol ())
;; (ex-put-example 'point-at-eol ())

;; register
(ex-put-example 'point-to-register '("(point-to-register ?r)"
                                     "(jump-to-register ?r)"))
(ex-put-example 'get-register '())
(ex-put-example 'set-register '())
(ex-put-example 'view-register '())
(ex-put-example 'insert-register '())


(ex-put-example 'intern-soft '(
  "(unintern \"frazzle\")"
  "(intern-soft \"frazzle\")        ; No such symbol exists."
  "(make-symbol \"frazzle\")        ; Create an uninterned one."
  "(intern-soft \"frazzle\")        ; That one cannot be found."
  "(setq __sym (intern \"frazzle\"))  ; Create an interned one."
  "(intern-soft \"frazzle\")       ; That one can be found!"
  "(eq __sym 'frazzle)              ; And it is the same one."))

;; property list

(ex-put-example 'symbol-plist '("(setplist 'foo '(a 1 b (2 3) c nil))"
  "(symbol-plist 'foo)"))
(ex-put-example 'setplist '(
  "(setplist 'foo '(a 1 b (2 3) c nil))"
  "(symbol-plist 'foo)"
  "(setplist 'pfoo1 '(:bar \"BAR\" :bazz \"BAZZ\"))"
  "(get 'pfoo1 :bar)"
  "(get 'pfoo1 :bazz)"))

(ex-put-example 'get '("(put 'fly 'verb 'transitive)"
  "(get 'fly 'verb)"
  "(put 'fly 'noun '(a buzzing little bug))"
  "(get 'fly 'noun)"
  "(symbol-plist 'fly)"))

(ex-put-example 'put '("(put 'fly 'verb 'transitive)"
  "(put 'fly 'noun '(a buzzing little bug))"
  "(get 'fly 'verb)"
  "(symbol-plist 'fly)"))

(ex-put-example 'plist-get '("(plist-get '(foo 4) 'foo)"
  "(plist-get '(foo 4 bad) 'foo)"
  "(plist-get '(foo 4 bad) 'bad)"
  "(plist-get '(foo 4 bad) 'bar)
;; ==="
  "(setq __pfoo '(:bar \"BAR\" :hoge \"HOGE\"))"
  "(plist-get __pfoo :bar)"))

(ex-put-example 'plist-put '("(setq my-plist '(bar t foo 4))"
  "(setq my-plist (plist-put my-plist 'foo 69))"
  "(setq my-plist (plist-put my-plist 'quux '(a)))"))

(ex-put-example 'make-symbol '(
  "(unintern 'frazzle)"
  "(intern-soft \"frazzle\")"
  "(make-symbol \"frazzle\")"
  "(intern-soft \"frazzle\")"
  "(setq __sym (intern \"frazzle\"))"
  "(intern-soft \"frazzle\")"
  "(eq __sym 'frazzle)"
))

(provide 'elisp-examples)

;;; end elisp-examples.el
