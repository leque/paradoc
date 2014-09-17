;;;
;;; Copyright (c) 2014 OOHASHI Daichi,
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;;
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; 3. Neither the name of the authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this
;;;    software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module zipper
  (export zipper zipper? zipper-current
          sexp->zipper zipper->sexp
          ;; predicates
          zipper-root? zipper-node? zipper-leaf?
          zipper-list-node? zipper-vector-node? zipper-pair-node?
          zipper-in-list? zipper-in-vector? zipper-in-pair?
          zipper-has-parent? zipper-has-left? zipper-has-right?
          ;; moving
          zipper-root zipper-parent
          zipper-car zipper-cdr
          zipper-child zipper-last-child
          zipper-first zipper-last
          zipper-left zipper-right
          ;; modifying
          zipper-map zipper-replace
          ;; insertion
          zipper-cons-child zipper-snoc-child
          zipper-insert zipper-insert-all
          zipper-insert-right zipper-insert-right-all
          ;; removing
          zipper-remove
          ;; insertion + moving
          zipper-cons-child* zipper-snoc-child*
          zipper-insert* zipper-insert-all*
          zipper-insert-right* zipper-insert-right-all*
          ;; removing + moving
          zipper-remove*
          )
  (use srfi-1)
  (use srfi-11)
  (use gauche.record)
  (use util.match))

(select-module zipper)

;; zipper for sexps
;;
;; type sexp ::= (Rec x
;;                (U Boolean
;;                   Number
;;                   Character
;;                   String
;;                   Symbol
;;                   Nil
;;                   (Pairof x x)
;;                   (Vectorof x)))
(define-record-type zipper
    (make-zipper current contexts)
    zipper?
  ;; the current element
  (current zipper-current)
  ;; outer elements
  (contexts zipper-contexts))

(define-record-type zipper-car-context
    make-zipper-car-context
    zipper-car-context?
  (cdr zipper-car-context-cdr))

(define-record-type zipper-cdr-context
    make-zipper-cdr-context
    zipper-cdr-context?
  (car zipper-cdr-context-car))

(define-record-type zipper-list-context
    make-zipper-list-context
    zipper-list-context?
  (left zipper-list-context-left)
  (right zipper-list-context-right))

(define-record-type zipper-vector-context
    make-zipper-vector-context
    zipper-vector-context?
  (left zipper-vector-context-left)
  (right zipper-vector-context-right))

;; Create a zipper for the given value.
(define (sexp->zipper x)
  (make-zipper x '()))

;; Extract a value from the zipper.
(define (zipper->sexp z)
  (zipper-current (zipper-root z)))

;; Return #t if the zipper focuses on the root element.
(define (zipper-root? z)
  (null? (zipper-contexts z)))

;; Return #t if the zipper has a parent element, otherwise #f.
(define (zipper-has-parent? z)
  (not (zipper-root? z)))

(define (non-empty-vector? x)
  (and (vector? x)
       (> (vector-length x) 0)))

;; Return #t if the zipper focuses on a non-null list,
;; a non-empty vector, or a pair,
;; otherwise #f.
(define (zipper-node? z)
  (let ((cur (zipper-current z)))
    (or (pair? cur)
        (non-empty-vector? cur))))

;; Return #t if the zipper focuses on an atom, otherwise #f.
(define (zipper-leaf? z)
  (not (zipper-node? z)))

;; Return #t if the zipper focuses on a proper non-null list, otherwise #f.
(define (zipper-list-node? z)
  (let ((cur (zipper-current z)))
    (and (not (null? cur))
         (proper-list? cur))))

;; Return #t if the zipper focuses on a non-empty vector.
(define (zipper-vector-node? z)
  (non-empty-vector? (zipper-current z)))

;; Return #t if the zipper focuses on a pair, otherwise #f.
(define (zipper-pair-node? z)
  (pair? (zipper-current z)))

(define (zipper-current-context z)
  (let ((contexts (zipper-contexts z)))
    (and (not (null? contexts))
         (car contexts))))

;; Return #t if the zipper is in a list, otherwise #f.
(define (zipper-in-list? z)
  (zipper-list-context? (zipper-current-context z)))

;; Return #t if the zipper is in a vector, otherwise #f.
(define (zipper-in-vector? z)
  (zipper-vector-context? (zipper-current-context z)))

;; Return #t if the zipper is in a pair, otherwise #f.
(define (zipper-in-pair? z)
  (let ((c (zipper-current-context z)))
    (or (zipper-car-context? c)
        (zipper-cdr-context? c))))

;; Return #t if the zipper can go left, otherwise #f.
(define (zipper-has-left? z)
  (match (zipper-contexts z)
    (((or ($ zipper-list-context (_ . _) _)
          ($ zipper-vector-context (_ . _) _))
      . _)
     #t)
    (_
     #f)))

;; Return #t if the zipper can go right, otherwise #f.
(define (zipper-has-right? z)
  (match (zipper-contexts z)
    (((or ($ zipper-list-context _ (_ . _))
          ($ zipper-vector-context _ (_ . _)))
      . _)
     #t)
    (_
     #f)))

;; Focus on the root element.
(define (zipper-root z)
  (if (zipper-root? z)
      z
      (zipper-root (zipper-parent z))))

;; Focus on the parent element of the current element of the zipper.
;; The zipper must be in a list, a vector, or a pair.
(define (zipper-parent z)
  (let ((cur (zipper-current z)))
    (define (f make-context conv left right ps)
      (make-zipper (conv (append-reverse left (cons cur right)))
                   ps))
    (match (zipper-contexts z)
      ((($ zipper-car-context x) . ps)
       (make-zipper (cons cur x) ps))
      ((($ zipper-cdr-context x) . ps)
       (make-zipper (cons x cur) ps))
      ((($ zipper-list-context left right) . ps)
       (f make-zipper-list-context values left right ps))
      ((($ zipper-vector-context left right) . ps)
       (f make-zipper-vector-context list->vector left right ps))
      (()
       (error "cannot go up from the root" (zipper-current z))))))

;; Focus on the car of the current element of the zipper.
;; The current element must be a pair.
(define (zipper-car z)
  (let ((cur (zipper-current z)))
    (if (pair? cur)
        (make-zipper (car cur)
                     (cons (make-zipper-car-context (cdr cur))
                           (zipper-contexts z)))
        (error "a pair required, but got" cur))))

;; Focus on the cdr of the current element of the zipper.
;; The current element must be a pair.
(define (zipper-cdr z)
  (let ((cur (zipper-current z)))
    (if (pair? cur)
        (make-zipper (cdr cur)
                     (cons (make-zipper-cdr-context (car cur))
                           (zipper-contexts z)))
        (error "a pair required, but got" cur))))

;; Focus on the n-th element of the current element of the zipper.
;; The current element must be a list or a vector.
(define (zipper-child z :optional (n 0))
  (define (rev-split xs n)
    (let loop ((n n)
               (ls '())
               (rs xs))
      (cond ((zero? n)
             (values ls rs))
            (else
             (loop (- n 1)
                   (cons (car rs) ls)
                   (cdr rs))))))
  (%zipper-child z (cute rev-split <> n)))

;; Focus on the last element of the current element of the zipper.
;; The current element must be a list or a vector.
(define (zipper-last-child z)
  (define (rev-but-last xs)
    (let ((rs (reverse xs)))
      (values (cdr rs) (list (car rs)))))
  (%zipper-child z rev-but-last))

(define (%zipper-child z split)
  (define (f x)
    (cond
     ((pair? x)
      (values make-zipper-list-context
              x))
     ((vector? x)
      (values make-zipper-vector-context
              (vector->list x)))
     (else
      (error "a list or a vector required, but got" x))))
  (let*-values (((cur) (zipper-current z))
                ((make-context elems) (f cur))
                ((ls rs) (split elems)))
    (make-zipper (car rs)
                 (cons (make-context ls (cdr rs))
                       (zipper-contexts z)))))

;; Focus on the left-most sibling of the current element of the zipper.
;; The zipper must be in a list or a vector.
(define (zipper-first z)
  (let ((cur (zipper-current z)))
    (define (f make-context left right ps)
      (if (null? left)
          z
          (let ((xs (append-reverse left right)))
           (make-zipper (car xs)
                        (xcons ps (make-context '() (cdr xs)))))))
    (match (zipper-contexts z)
      ((($ zipper-list-context left right) . ps)
       (f make-zipper-list-context left right ps))
      ((($ zipper-vector-context left right) . ps)
       (f make-zipper-vector-context left right ps))
      (_
       (error "not in a list or a vector:" (zipper-current z))))))

;; Focus on the right-most sibling of the current element of the zipper.
;; The zipper must be in a list or a vector.
(define (zipper-last z)
  (let ((cur (zipper-current z)))
    (define (f make-context left right ps)
      (if (null? right)
          z
          (let ((xs (append-reverse right left)))
            (make-zipper (car xs)
                         (xcons ps (make-context (cdr xs) '()))))))
    (match (zipper-contexts z)
      ((($ zipper-list-context left right) . ps)
       (f make-zipper-list-context left right ps))
      ((($ zipper-vector-context left right) . ps)
       (f make-zipper-vector-context left right ps))
      (_
       (error "not in a list or an vector" (zipper-current z))))))

;; Focus on the left sibling of the current element of the zipper.
;; The zipper must be in a list or a vector.
(define (zipper-left z)
  (let ((cur (zipper-current z)))
    (define (f make-context left right ps)
      (if (null? left)
          (error "no more left elements" (zipper-current z))
          (make-zipper (car left)
                       (xcons ps (make-context (cdr left) (cons cur right))))))
    (match (zipper-contexts z)
      ((($ zipper-list-context left right) . ps)
       (f make-zipper-list-context left right ps))
      ((($ zipper-vector-context left right) . ps)
       (f make-zipper-vector-context left right ps))
      (_
       (error "not in a list or a vector:" (zipper-current z))))))

;; Focus on the right sibling of the current element of the zipper.
;; The zipper must be in a list or a vector.
(define (zipper-right z)
  (let ((cur (zipper-current z)))
    (define (f make-context left right ps)
      (if (null? right)
          (error "no more right elements" (zipper-current z))
          (make-zipper (car right)
                       (xcons ps (make-context (cons cur left)
                                               (cdr right))))))
    (match (zipper-contexts z)
      ((($ zipper-list-context left right) . ps)
       (f make-zipper-list-context left right ps))
      ((($ zipper-vector-context left right) . ps)
       (f make-zipper-vector-context left right ps))
      (_
       (error "not in a list or an vector" (zipper-current z))))))

;; Map f over the current element of the zipper.
(define (zipper-map f z)
  (make-zipper (f (zipper-current z))
               (zipper-contexts z)))

;; Replace the current element of the zipper with a given value.
;; This function is equivalent to (zipper-map (constantly x) z).
(define (zipper-replace z x)
  (make-zipper x (zipper-contexts z)))

(define (cannot-insert z x)
  (error "cannot insert to an atom" (zipper-current z)))

(define (zipper-cons-child z x)
  (zipper-map (lambda (v)
                (cond ((or (pair? v) (null? v))
                       (cons x v))
                      ((vector? v)
                       (list->vector (cons x (vector->list v))))
                      (else (cannot-insert z x))))
              z))

(define (zipper-cons-child* z x)
  (let ((cur (zipper-current z)))
    (define (f make-context conv)
      (make-zipper x
                   (cons (make-context '() (conv cur))
                         (zipper-contexts z))))
    (cond ((or (pair? cur) (null? cur))
           (f make-zipper-list-context values))
          ((vector? cur)
           (f make-zipper-vector-context vector->list))
          (else
           (cannot-insert z x)))))

(define (zipper-snoc-child z x)
  (zipper-map (lambda (cur)
                (cond ((or (pair? cur) (null? cur))
                       (append cur (list x)))
                      ((vector? cur)
                       (list->vector
                        (append (vector->list cur) (list x))))
                      (else
                       (cannot-insert z x))))
              z))

(define (zipper-snoc-child* z x)
  (let ((cur (zipper-current z)))
    (define (f make-context conv)
      (make-zipper x
                   (cons (make-context (reverse (conv cur)) '())
                         (zipper-contexts z))))
    (cond ((or (pair? cur) (null? cur))
           (f make-zipper-list-context values))
          ((vector? cur)
           (f make-zipper-vector-context vector->list))
          (else
           (cannot-insert z x)))))

;; Insert a value to the zipper without moving.
;; The zipper must be in a list or a vector.
(define (zipper-insert z x)
  (%zipper-insert z x #f))

;; Insert a value to the zipper.
;; The zipper must be in a list or a vector.
;; The current element of the zipper is shifted to right.
(define (zipper-insert* z x)
  (%zipper-insert z x #t))

(define (%zipper-insert z x move)
  (define (f make-context left right ps)
    (let ((cur (zipper-current z)))
      (if move
          (make-zipper x (xcons ps (make-context left (cons cur right))))
          (make-zipper cur (xcons ps (make-context (cons x left) right))))))
  (match (zipper-contexts z)
    ((($ zipper-list-context left right) . ps)
     (f make-zipper-list-context left right ps))
    ((($ zipper-vector-context left right) . ps)
     (f make-zipper-vector-context left right ps))
    (_
     (cannot-insert z x))))

;; Insert values to the zipper without moving.
;; The zipper must be in a list of a vector.
(define (zipper-insert-all z xs)
  (%zipper-insert-all z xs #f))

;; Insert values to the zipper.
;; The zipper must be in a list of a vector.
;; The current element of the resulted zipper become the (car xs).
(define (zipper-insert-all* z xs)
  (%zipper-insert-all z xs #t))

(define (%zipper-insert-all z xs move)
  (define (f make-context left right ps)
    (let ((cur (zipper-current z)))
      (if move
          (make-zipper (car xs)
                       (xcons ps (make-context left
                                               (append (cdr xs)
                                                       (cons cur right)))))
          (make-zipper cur
                       (xcons ps (make-context (append-reverse xs left)
                                               right))))))
  (if (null? xs)
      z
      (match (zipper-contexts z)
        ((($ zipper-list-context left right) . ps)
         (f make-zipper-list-context left right ps))
        ((($ zipper-vector-context left right) . ps)
         (f make-zipper-vector-context left right ps))
        (_
         (cannot-insert z xs)))))

(define (zipper-insert-right z x)
  (%zipper-insert-right z x #f))

(define (zipper-insert-right* z x)
  (%zipper-insert-right z x #t))

(define (%zipper-insert-right z x move)
  (define (f make-context left right ps)
    (let ((cur (zipper-current z)))
      (if move
          (make-zipper x (xcons ps (make-context (cons cur left) right)))
          (make-zipper cur (xcons ps (make-context left (cons x right)))))))
  (match (zipper-contexts z)
    ((($ zipper-list-context left right) . ps)
     (f make-zipper-list-context left right ps))
    ((($ zipper-vector-context left right) . ps)
     (f make-zipper-vector-context left right ps))
    (_
     (cannot-insert z x))))

(define (zipper-insert-right-all z xs)
  (%zipper-insert-right-all z xs #f))

(define (zipper-insert-right-all* z xs)
  (%zipper-insert-right-all z xs #t))

(define (%zipper-insert-right-all z xs move)
  (define (f make-context left right ps)
    (let ((cur (zipper-current z)))
      (if move
          (make-zipper (car xs)
                       (xcons ps (make-context (cons cur left)
                                               (append (cdr xs) right))))
          (make-zipper cur
                       (xcons ps (make-context left
                                               (append xs right)))))))
  (if (null? xs)
      z
      (match (zipper-contexts z)
        ((($ zipper-list-context left right) . ps)
         (f make-zipper-list-context left right ps))
        ((($ zipper-vector-context left right) . ps)
         (f make-zipper-vector-context left right ps))
        (_
         (cannot-insert z xs)))))

;; Remove the current element of the zipper.
;; The zipper must be in a list of a vector.
;; The focus moves to the parent.
(define (zipper-remove z)
  (%zipper-remove z #f))

;; Remove the current element of the zipper.
;; The zipper must be in a list of a vector.
;; The focus moves to the right, the left, or the parent.
(define (zipper-remove* z)
  (%zipper-remove z #t))

(define (%zipper-remove z smart)
  (define (f make-context left right ps conv)
    (cond
     ((and smart (pair? right))
      (make-zipper (car right)
                   (xcons ps (make-context left (cdr right)))))
     ((and smart (pair? left))
      (make-zipper (car left)
                   (xcons ps (make-context (cdr left) right))))
     (else
      (zipper-replace (zipper-parent z)
                      (conv (append-reverse left right))))))
  (match (zipper-contexts z)
    (((or ($ zipper-car-context x)
          ($ zipper-cdr-context x))
      . ps)
     (make-zipper x ps))
    ((($ zipper-list-context left right) . ps)
     (f make-zipper-list-context left right ps values))
    ((($ zipper-vector-context left right) . ps)
     (f make-zipper-vector-context left right ps list->vector))
    (()
     (error "cannot remove a root element" (zipper-current z)))))
