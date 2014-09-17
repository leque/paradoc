(use gauche.test)
(use zipper)

(test-start "zipper")

(test-module 'zipper)

(define-syntax >>
  (syntax-rules ()
    ((>> x f)
     (f x))
    ((>> x f g ...)
     (>> (f x) g ...))))

(use srfi-1)

(let ((vs (iota 6)))
  (test* "sexp->zipper" #t (zipper? (sexp->zipper vs)))
  (test* "zipper-current"
         vs
         (zipper-current (sexp->zipper vs)))

  (test* "zipper-child"
         (car vs)
         (zipper-current (zipper-child (sexp->zipper vs))))

  (dolist (n (iota 5))
    (test* (format "zipper-child(~A)" n)
           n
           (zipper-current (zipper-child (sexp->zipper vs) n))))

  (test* "zipper-car"
         (car vs)
         (>> vs
             sexp->zipper
             zipper-car
             zipper-current))

  (test* "zipper-cdr"
         (cdr vs)
         (>> vs
             sexp->zipper
             zipper-cdr
             zipper-current))

  (let ((v 'x))
    (test* "zipper-replace"
           v
           (>> vs
               sexp->zipper
               (cut zipper-replace <> v)
               zipper->sexp)))

  (test* "zipper-replace"
         '(0 1 2 a 4 5)
         (>> vs
             sexp->zipper
             (cut zipper-child <> 3)
             (cut zipper-replace <> 'a)
             zipper->sexp))

  (test* "zipper-replace"
         '(0 1 2 a 4 b)
         (>> vs
             sexp->zipper
             (cut zipper-child <> 3)
             (cut zipper-replace <> 'a)
             zipper-parent
             zipper-last-child
             (cut zipper-replace <> 'b)
             zipper->sexp))

  (test* "zipper-insert"
         '(a 0 1 2 3 4 5)
         (>> vs
             sexp->zipper
             zipper-child
             (cut zipper-insert <> 'a)
             zipper->sexp))

  (test* "zipper-insert"
         '(0 1 2 a 3 4 5)
         (>> vs
             sexp->zipper
             (cut zipper-child <> 3)
             (cut zipper-insert <> 'a)
             zipper->sexp))

  (test* "zipper-insert"
         '(a b 0 1 2 3 4 5)
         (>> vs
             sexp->zipper
             zipper-child
             (cut zipper-insert <> 'a)
             (cut zipper-insert <> 'b)
             zipper->sexp))

  (test* "zipper-insert-all"
         '(a b 0 1 2 3 4 5)
         (>> vs
             sexp->zipper
             zipper-child
             (cut zipper-insert-all <> '(a b))
             zipper->sexp))

  (test* "zipper-insert-right"
         '(0 a 1 2 3 4 5)
         (>> vs
             sexp->zipper
             zipper-child
             (cut zipper-insert-right <> 'a)
             zipper->sexp))

  (test* "zipper-insert-right"
         '(0 1 2 3 a 4 5)
         (>> vs
             sexp->zipper
             (cut zipper-child <> 3)
             (cut zipper-insert-right <> 'a)
             zipper->sexp))

  (test* "zipper-insert-right"
         '(0 b a 1 2 3 4 5)
         (>> vs
             sexp->zipper
             zipper-child
             (cut zipper-insert-right <> 'a)
             (cut zipper-insert-right <> 'b)
             zipper->sexp))

  (test* "zipper-insert-right-all"
         '(0 a b 1 2 3 4 5)
         (>> vs
             sexp->zipper
             zipper-child
             (cut zipper-insert-right-all <> '(a b))
             zipper->sexp))

  (test* "zipper-remove (list)"
         '(0 1 2 4 5)
         (>> vs
             sexp->zipper
             (cut zipper-child <> 3)
             zipper-remove
             zipper->sexp))

  (test* "zipper-remove (vector)"
         '#(1 2 3 4 5)
         (>> vs
             list->vector
             sexp->zipper
             zipper-child
             zipper-remove
             zipper->sexp))

  (test* "zipper-remove (car)"
         (cdr vs)
         (>> vs
             sexp->zipper
             zipper-car
             zipper-remove
             zipper->sexp))

  (test* "zipper-remove (cdr)"
         (car vs)
         (>> vs
             sexp->zipper
             zipper-cdr
             zipper-remove
             zipper->sexp))
  )

(test-end)
