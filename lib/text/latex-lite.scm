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

(define-module text.latex-lite
  (export latex-escape-string
          latex:documentclass latex:usepackage
          latex:author latex:title latex:date
          latex:document
          latex:maketitle latex:tableofcontents
          latex:listoffigures latex:listoftables
          latex:newpage
          latex:section latex:subsection latex:subsubsection
          latex:par
          latex:rule
          latex:itemize latex:enumerate latex:description
          latex:item latex:item*
          latex:quotation latex:verbatim
          latex:center
          latex:label latex:ref
          latex:textrm latex:textit latex:textbf latex:texttt latex:textsl
          latex:emph
          latex:verb
          latex:figure latex:caption
          latex:centering
          latex:$
          latex:table latex:tabular latex:hline
          latex:multicolumn
          ;; pTeX
          latex:textmc latex:textgt
          ;; from hyperref
          latex:hyperdef latex:hyperref latex:autoref latex:nameref
          latex:url latex:href latex:hypertarget latex:hyperlink
          ;; from graphicx
          latex:includegraphics
          ;; from booktabs
          latex:toprule latex:midrule latex:bottomrule
          ;; from tabularx
          latex:tabularx
          ;; from longtable
          latex:longtable
          latex:endhead latex:endfirsthead latex:endfoot latex:endlastfoot
          ;; from listings
          latex:lstlisting latex:lstinputlisting
          )
  (use srfi-13))

(select-module text.latex-lite)

(define (latex-escape-string str
                             :key
                             (escape-space #f)
                             (escape-newline 'space))
  (with-output-to-string
    (lambda ()
      (string-for-each
       (lambda (c)
         (case c
           ((#\& #\% #\$ #\# #\_ #\{ #\})
            (display #\\)
            (display c))
           ((#\~)
            (display "\\textasciitilde{}"))
           ((#\^)
            (display "\\textasciicircum{}"))
           ((#\\)
            (display "\\textbackslash{}"))
           ((#\space)
            (display (if escape-space
                         "\\quad{}"
                         c)))
           ((#\newline)
            (display (case escape-newline
                       ((pre) "\\mbox{} \\ ")
                       ((space) " ")
                       (else c))))
           (else
            (display c))))
       str))))

(define (latex:documentclass name . opts)
  `("\\documentclass[" ,(intersperse "," opts) "]{" ,name "}\n"))

(define (latex:usepackage name . opts)
  `("\\usepackage[" ,(intersperse "," opts) "]{" ,name "}\n"))

(define-syntax define-latex-environment
  (syntax-rules ()
    ((_ name env-name)
     (define name
       (let ((env-name (symbol->string 'env-name)))
         (lambda args
           `("\n\\begin{" env-name "}\n"
             ,args
             "\\end{" env-name "}%\n")))))))

(define-syntax define-latex-environments
  (syntax-rules ()
    ((_ (name env-name) ...)
     (begin
       (define-latex-environment name env-name)
       ...))))

(define-syntax define-simple-latex-command
  (syntax-rules ()
    ((_ name command-name)
     (define name
       (let ((cmd (symbol->string 'command-name)))
         (lambda args
           `("\\" ,cmd "{" ,args "}")))))))

(define-syntax define-simple-latex-commands
  (syntax-rules ()
    ((_ (name command-name) ...)
     (begin
       (define-simple-latex-command name command-name)
       ...))))

(define-syntax define-no-arg-latex-commands
  (syntax-rules ()
    ((_ (name command-name) ...)
     (begin
       (define (name)
         `("\\" ,(symbol->string 'command-name)))
       ...))))

(define-latex-environments
  (latex:document document)
  (latex:itemize itemize)
  (latex:enumerate enumerate)
  (latex:description description)
  (latex:quotation quotation)
  (latex:verbatim verbatim)
  (latex:center center)
  )

(define (latex:par . args)
  `("\n\n"
    ,args
    "\n\n"))

(define-simple-latex-commands
  (latex:author author)
  (latex:title title)
  (latex:date date)
  (latex:section section)
  (latex:subsection subsection)
  (latex:subsubsection subsubsection)
  (latex:label label)
  (latex:ref ref)
  (latex:textrm textrm)
  (latex:textit textit)
  (latex:textbf textbf)
  (latex:texttt texttt)
  (latex:textsl textsl)
  (latex:textmc textmc)
  (latex:textgt textgt)
  (latex:emph emph)
  )

(define (latex:verb open&close . args)
  (receive (open close) (if (pair? open&close)
                            (car+cdr open&close)
                            (values open&close open&close))
    `("\\verb" open args close)))

(define (latex:$ . args)
  `("$" ,args "$"))

(define-no-arg-latex-commands
  (latex:maketitle maketitle)
  (latex:tableofcontents tableofcontents)
  (latex:listoffigures listoffigures)
  (latex:listoftables listoftables)
  )


(define-no-arg-latex-commands
  (latex:newpage newpage)
  )

(define (latex:item . args)
  `("\\item{" ,args "}\n"))

(define (latex:item* label . args)
  `("\\item[" ,label "] {" ,args "}\n"))

(define (latex:rule :optional (width "\\linewidth"))
  `("\n\\rule{" ,width "}\n"))

(define (latex:hyperdef category name . text)
  `("\\hyperdef{" ,category "}{" ,name "}{" ,text "}"))

(define (latex:hyperref label text)
  `("\\hyperref[" ,label "]{" ,text "}"))

(define (latex:autoref label)
  `("\\autoref{" ,label "}"))

(define (latex:nameref label)
  `("\\nameref{" ,label "}"))

(define (latex:url url)
  `("\\url{" ,url "}"))

(define (latex:href url . args)
  `("\\href{" ,url "}{" ,args "}"))

(define (latex:hypertarget target . args)
  `("\\hypertarget{" ,target "}{" ,args "}"))

(define (latex:hyperlink target . args)
  `("\\hyperlink{" ,target "}{" ,args "}"))

(define (latex:includegraphics filename
                               :key
                               (width #f)
                               (height #f)
                               (keepaspectratio #f)
                               (scale #f)
                               (bb #f)
                               (hiresbb #f)
                               (clip #f)
                               (trim #f)
                               (angle #f)
                               (draft #f))
  `("\\includegraphics["
    ,(intersperse ","
                  (cond-list
                   (width `("width=" ,width))
                   (height `("height=" ,height))
                   (keepaspectratio "keepaspectratio")
                   (scale `("scale=" ,scale))
                   (hiresbb "hiresbb")
                   (clip "clip")
                   (trim `("trim=" ,(intersperse " " trim)))
                   (angle `("angle=" ,angle))
                   (draft "draft")))
    "]{" ,filename "}")
  )

;; (put 'latex:figure 'scheme-indent-function 1)
(define (latex:figure pos . args)
  `("\\begin{figure}[" ,pos "]%\n"
    ,args
    "\\end{figure}%\n"))

(define (latex:caption short . args)
  (if short
      `("\\caption[" ,short "]{" ,args "}")
      `("\\caption{" ,args "}")))

(define (latex:centering)
  `("\\centering{}"))

;; (put 'latex:table 'scheme-indent-function 1)
(define (latex:table pos . args)
  `("\\begin{table}[" ,pos "]\n"
    ,args
    "\\end{table}%\n"))

;; (put 'latex:tabular 'scheme-indent-function 1)
(define (latex:tabular cols . args)
  `("\\begin{tabular}{" ,cols "}\n"
    ,args
    "\\end{tabular}%\n"))

;; (put 'latex:longtable 'scheme-indent-function 1)
(define (latex:longtable cols . args)
  `("\\begin{longtable}{" ,cols "}\n"
    ,args
    "\\end{longtable}%\n"))

(define-no-arg-latex-commands
  (latex:hline hline)
  (latex:toprule toprule)
  (latex:midrule midrule)
  (latex:bottomrule bottomrule)
  (latex:endhead endhead)
  (latex:endfirsthead endfirsthead)
  (latex:endfoot endfoot)
  (latex:endlastfoot endlastfoot)
  )

(define (latex:multicolumn n col . args)
  `("\\multicolumn{" ,n "}{" ,col "}{" ,args "}"))

;; (put 'latex:tabularx 'scheme-indent-function 2)
(define (latex:tabularx width cols . args)
  `("\\begin{tabular}{" ,width "}{" ,cols "}\n"
    ,args
    "\\end{tabular}%\n"))

(define (latex:lstlisting body :key (language #f))
  (let ((opts (cond-list (language
                          (format "language=~A" language)))))
    `("\\begin{lstlisting}[" ,opts "]\n"
      ,body
      "\\end{lstlisting}")))

(define (latex:lstinputlisting filename :key (language #f))
  (let ((opts (cond-list (language
                          (format "language=~A" language)))))
    `("\\lstinputlisting[" ,opts "]{" ,filename "}")))
