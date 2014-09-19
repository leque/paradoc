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

(define-module scribble
  (export scribble-read
          scribble-parse
          scribble-parse*
          scribble-parse-implicit-para
          scribble-eval

          &scribble-parse-error scribble-parse-error?
          scribble-parse-error-port-name
          scribble-parse-error-line
          scribble-parse-error

          &scribble-syntax-error scribble-syntax-error?
          scribble-syntax-error-form
          scribble-syntax-error

          define-scribble-macro
          define-inline-scribble-macro
          scribble:with-module
          scribble:current-module

          scribble-module
          make-scribble-module
          scribble-module?
          scribble-module-name
          scribble-module-template scribble-module-set-template!
          scribble-module-macros scribble-module-set-macros!
          )
  (use srfi-1)
  (use srfi-13)
  (use gauche.parameter)
  (use gauche.record)
  (use text.tree)
  (use util.match)
  )

(select-module scribble)

(define-syntax import-define
  (syntax-rules ()
    ((_ mod name ...)
     (begin
       (define name (with-module mod name))
       ...))))

(import-define gauche.internal pair-attribute-set! extended-cons)

(define-condition-type &scribble-parse-error &error
  scribble-parse-error?
  (port-name scribble-parse-error-port-name)
  (line scribble-parse-error-line))

(define (scribble-parse-error port line fmt . args)
  (raise (condition
          (&message
           (message (apply format fmt args)))
          (&scribble-parse-error
           (port-name (port-name port))
           (line line)))))

(define-condition-type &scribble-syntax-error &error
  scribble-syntax-error?
  (form scribble-syntax-error-form))

(define (scribble-syntax-error form fmt . args)
  (raise (condition
          (&message
           (message (apply format fmt args)))
          (&scribble-syntax-error
           (form form)))))

(define (attach-debug-source-info port line x)
  (cond ((not (pair? x))
         x)
        (else
         (rlet1 pair (extended-cons (car x)
                                    (cdr x))
           (pair-attribute-set! pair
                                'source-info
                                (list (port-name port) line))))))

(include "scribble/internal/scribble.scm")

(set! default-ecape-char #\@)

(define (scribble-parse* input :optional (para 'para))
  (scribble-parse-implicit-para (scribble-parse input) para))

(define (scribble-parse-implicit-para forms :optional (para 'para))
  (define (inline? x)
    (or (string? x)
        (and (pair? x)
             (scribble-inline-macro? (car x)))))
  (define (trim xs)
    (drop-while (cut equal? <> "\n") xs))
  (define (normal forms rs)
    (match (values forms)
      (()
       (reverse! rs))
      (("\n" "\n" . rest)
       (paragraph rest rs '()))
      (((? inline? x) . rest)
       (paragraph rest rs  (list x)))
      ((s . rest)
       (normal rest (cons s rs)))))
  (define (paragraph forms rs ps)
    (define (tl)
      (let ((ps (trim (reverse! (trim ps)))))
        (if (null? ps)
            rs
            (cons `(,para ,@ps) rs))))
    (match forms
      (()
       (reverse! (tl)))
      (("\n" "\n" . rest)
       (normal rest (tl)))
      (((? inline? x) . rest)
       (paragraph rest rs (cons x ps)))
      ((s . rest)
       (normal forms (tl)))))
  (normal (trim forms) '()))

;; (put 'scribble:with-module 'scheme-indent-function 1)
(define-syntax scribble:with-module
  (syntax-rules ()
    ((_ mod . body)
     (parameterize ((scribble:current-module mod))
       . body))))

(define (scribble-macro-binding name)
  (assv name (scribble-module-macros (scribble:current-module))))

(define (scribble-macro name)
  (cond ((scribble-macro-binding name)
         => caddr)
        (else #f)))

(define (scribble-inline-macro? name)
  (cond ((scribble-macro-binding name)
         => cadr)
        (else #f)))

(define (scribble-eval scrbl)
  (cond ((and (pair? scrbl)
              (symbol? (car scrbl)))
         (cond
          ((scribble-macro (car scrbl))
           => (lambda (proc)
                (let ((exp (guard (exc
                                   (else
                                    (scribble-syntax-error
                                     scrbl
                                     "error while expanding macro: ~A: ~A"
                                     (car scrbl)
                                     (if (condition-has-type? exc &message)
                                         (condition-ref exc 'message)
                                         ""))))
                             (apply proc (cdr scrbl)))))
                  (scribble-eval exp))))
          (else
           (scribble-syntax-error scrbl
                                  "undefined macro: ~A"
                                  (car scrbl)))))
        ((pair? scrbl)
         (map scribble-eval scrbl))
        (else scrbl)))

(define scribble:current-module
  (make-parameter #f))

(define-record-type scribble-module
    (%make-scribble-module name template macros)
    scribble-module?
  (name scribble-module-name)
  (template scribble-module-template scribble-module-set-template!)
  (macros scribble-module-macros scribble-module-set-macros!))

(define (make-scribble-module name)
  (%make-scribble-module name (lambda (x) x) '()))

(define (%set-scribble-macro! env name inline? proc)
  (let ((cur-env (scribble:current-module)))
    (scribble-module-set-macros!
     cur-env
     (cons (list name inline? proc)
           (scribble-module-macros cur-env)))))

(define-syntax define-inline-scribble-macro
  (syntax-rules ()
    ((_ (name . args) expr . body)
     (%define-scribble-macro #t name () () args (expr . body)))))

(define-syntax define-scribble-macro
  (syntax-rules ()
    ((_ (name . args) expr . body)
     (%define-scribble-macro #f name () () args (expr . body)))))

(define (get-keyword! key kv-list fallback-proc)
  (define (keyword-symbol? x)
    (and (symbol? x)
         (string-suffix? ":" (symbol->string x))))
  (define (eqv?? x)
    (lambda (y)
      (eqv? x y)))
  (let loop ((head #f)
             (kvs kv-list))
    (match kvs
      (((? (eqv?? key)) v . rest)
       (cond (head
              (set-cdr! head rest)
              (values v kv-list))
             (else
              (values v rest))))
      (((not (? keyword-symbol?)) . _)
       (values (fallback-proc) kv-list))
      ((_ _ . rest)
       (loop (cdr kvs)
             (cddr kvs)))
      (_
       (values (fallback-proc) kv-list)))))

;; (put 'let-keys 'scheme-indent-function 2)
(define-syntax let-keys
  (syntax-rules ()
    ((_ kvs () expr . body)
     (let () expr . body))
    ((_ kvs ((var key default) . rest) . body)
     (receive (var more) (get-keyword! key kvs (lambda () default))
       (let-keys more
           rest
         . body)))
    ((_ kvs rest expr . body)
     (let ((rest kvs))
       expr . body))))

(define-syntax %define-scribble-macro
  (syntax-rules ()
    ((_ inline? name ((var key expr) ...) args () (body ...))
     (%set-scribble-macro!
      (scribble:current-module)
      'name
      inline?
      (lambda x
        (let-keys x
            ((var 'key expr) ...
             . rest)
          (apply (lambda args body ...) rest)))))
    ((_ inline? name (keys ...) () ((key var) . rest) bodies)
     (%define-scribble-macro
      inline?
      name
      ((var key (error "no arguments for " 'key)) keys ...)
      ()
      rest
      bodies))
    ((_ inline? name (keys ...) () ((key var expr) . rest) bodies)
     (%define-scribble-macro inline?
                             name
                             ((var key expr) keys ...)
                             ()
                             rest
                             bodies))
    ((_ inline? name keys () args bodies)
     (%define-scribble-macro inline? name keys args () bodies))))
