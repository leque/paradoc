@title{Example of Paradoc}
@author{}
@date{}

@section[tag: "first"]{Introduction}

Paradoc is a convertion tool from
@hyperlink["http://docs.racket-lang.org/scribble/index.html"]{Scribble}
to HTML or LaTeX.

@section{inline markups}

@desclist[
@item["@emph"]{@emph{emphasis}}
@item["@strong"]{@strong{strong emphasis}}
@item["@code"]{@code{code}}
@item["@superscript, @subscript"]{
@superscript{superscript} and @subscript{subscript}.
}
@item["@var"]{@var{variable}}
]

@section[tag: "links"]{links}

@"@"secref creates a link to section.
The name of the first section is @secref{first}.
@"@"elemtag creates @elemtag["anchor"]{an anchored element}.
@"@"elemref referrences @elemref["anchor"]{it}.
A link to @elemref["first"]{the first chapter} with @"@"elemref.
Paradoc does not offer automatic url links.
You must explicitly use @"@"url for an url link.
e.g. @url{http://www.google.com}.
@section{math}
@subsection{inline-math}

You can write equation with @"@"math in TeX syntax:
@math|{\sum_{i=1}^{n} n^2}|.

@subsection{block math}

@"@"math-block requires explicit @code|{\begin{align*}}| ...
@code|{\end{align*}}| etc.

@math-block{
\begin{align*}
\int_a^b f(x) \mathrm{d}x
\end{align*}
}

MathJax's markers in HTML, or $ in TeX does not affect elsewhere.
This is \(ordinary\) text.

@section{code block}

Factorial method in Ruby.

@code-block[language: Ruby]|{
def fact(n)
   if n == 0 then
      1
   else
      n * fact(n - 1)
   end
end
}|

@section{image}
You can include image with @"@"image and @"@"figure.
@image{imgs/lisplogo_alien_128.png}
These images come from @url{http://lisperati.com/logo.html}.

@figure[
  caption: "Cute Lisp alien"
  @image{imgs/lisplogo_warning_256.png}
]

@section{table}

Table syntax is not paren-agnostic though.

@(table
  caption: "scribble tags and HTML/LaTeX"

  ("Scribble" "HTML" "LaTeX")
  (
   ("@title" "title, h1" "title")
   ("@author" "meta" "author")
   ("@date" "meta" "date")
   ("@section" "h2" "section")
   ("@subsection" "h3" "subsection")
   ("@subsubsection" "h4" "subsubsection")
   ("@para (implicit)" "p" "(implicit)")
   ("@emph" "em" "textit")
   ("@strong" "strong" "textbf")
   ("@code" "code" "texttt")
   ("@var" "var" "mathit")
   ("@code-block" "pre.paradoc-code-block > code" "lstlisting")
   ("@include-code-block" "pre.paradoc-code-block > code" "lstinputlisting")
   ("@superscript" "sup" "$^{\\text{...}}$")
   ("@subscript" "sub" "$_{\\text{...}}$")
   ("@secref" "a[href]" "nameref")
   ("@elemtag" "span[id]" "hypertarget")
   ("@elemref" "a[href]" "hyperref")
   ("@url" "a[href]" "url")
   ("@hyperlink" "a[href]" "href")

   ("@math" "span.paradoc-math" "$ ... $")
   ("@math-blcok" "div.paradoc-math" "(passes arguments to LaTeX as is.)")
   ("@itemlist" "ul" "itemize")
   ("@itemlist[style: ordered]" "ol" "enumerate")
   ("@desclist" "dl" "description")
   ("@item (itemlist)" "li" "item")
   ("@item (desclist)" "dt, dd" "item")
   ("@hyperlink" "a" "href")
   ("@url" "a" "url")
   ("@figure" "div.figure, img" "figure")
   ("@image" "img" "includegraphics")
   ("@table" "table" "longtable")
  ))

@section{inclusion}

@include-section{dir/p.scrbl}

@include{dir/s.scm}

@section{custom macro}

You can load custom macros by passing @code{-l} option to @code{paradoc}.

@section{source code}

Source code of this document is shown below:

@include-code-block{example.scrbl}
