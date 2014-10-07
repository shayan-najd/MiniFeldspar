\documentclass{llncs}
%\documentclass[preprint]{sigplanconf}

%include polycode.fmt
%format == = "\longeq "
%format || = "||"
%format <|| = "\openq "
%format ||> = "\closeq "

% US Letter page size
%\pdfpagewidth=8.5in
%\pdfpageheight=11in


% The following \documentclass options may be useful:
%
% 10pt          To set in 10-point type instead of 9-point.
% 11pt          To set in 11-point type instead of 9-point.
% authoryear    To obtain author/year citation style instead of numeric.

\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{stmaryrd}
\usepackage{xspace}
\usepackage{hyperref}
\usepackage{url}
\usepackage{color}
\usepackage{listings}
\lstset{language=C,identifierstyle=\ttfamily,keywordstyle=\bfseries\ttfamily}
%% \usepackage[table]{xcolor}
%% \usepackage{colortbl}

%%% macros

\newcommand{\todo}[1]{{\noindent\small\color{red} \framebox{\parbox{\dimexpr\linewidth-2\fboxsep-2\fboxrule}{\textbf{TODO:} #1}}}}
%\newcommand{\todo}[1]{}

\newcommand{\longeq}{=\!=}
\newcommand{\openq}{[||||\,}
\newcommand{\closeq}{\,||||]}

%% Draft outline

%% (0) Abstract
%% (1) Keywords
%% (2) Introduction
%%     (i) very general introduction to the problem and the solution without
%%          (a) terms not understandable by a third year CS student
%%          (b) references
%%     (ii) general technical introduction to the problem we are trying to solve
%%     (iii) why the problem is important (why the problem matters)
%%     (iv) general technical introduction to the solution we are providing
%%     (v) our contributions (why our solution matters)
%%     (vi) paper layout
%% (3) Background
%%     (i) background on embedding including
%%         (a) introducing MiniFeldspar's first-order language, vectors and the unique benefits of it (free fusion and guarantees)
%%         (b) introducing the essence of the Practical Theory's embedding technique of SQL including what subformula property is and why it matters
%% (4) Our proposal
%%     (i) vector processing examples in MiniFeldspar's first-order language
%%     (ii) vector processing examples in TH language
%%     (iii) describing benefits of our approach in details (e.g. syntax, absence of vectors, and reusing the host language normalizer)
%%     (iv) stressing that normalizer does more optimization than evaluation in the spirit of MiniFeldspar's shallow embedding
%% (5) First Attempt (naive solution)
%%    describing the conversion, normalization, and why subformula property fails
%% (6) Second Attempt (real solution)
%%    describing the conversion with the FO constraint and theorems that subformula property holds
%% (7) Performance
%%    (i) examples of free fusion (e.g. scalarProd from MiniFeldspar) and showing that we do as good
%%    (ii) the table and discussions around the table
%% (8) Related Work
%% (9) Future Work
%% (10) Conclusion
%% (11) Acknowledgements

%%  Notes of First Meeting With Phil at ICFP
%% -----------------------------------------
%% Examples:
%% * sums and for loops
%% * Syntax
%%   - Conditional and less than
%%      + Haskell Only
%%      + Mixing host and embedded code
%%   - Deep and Shallow Embedding
%%      + simulates functions and tuples but not sums and for loops
%%      + types get more complicated
%% * Normalisation
%%   - ?
%% * Sharing
%%   - Observable sharing
%%   - CSE
%%   - Explicit sharing
%%
%%
%% ---------------
%% Possible Approaches:
%% 1. Implementations of ICFP'13 SQL, Lava, and Feldspar
%% 2. Compare Feldspar (deep + shallow + CSE + Opt.) with QDSL
%%    - Introduction: Compare Feldspar implemented with QDSL to Feldspar implemented with deep and
%%                    shallow embedding achieve the same effect with significantly less effort.
%%                    QDSL approach is straightforward and reuses a normaliser and typechecker.
%%                    In comparison, deep and shallow embedding requires
%%                      + repeating standard boilerplate for map between deep and shallow represnations
%%                      + more complicated type structures
%%                      + more complicated error messages
%%                      + implementing a CSE phase
%%                      + implementing more opt. (cost of special purpose optimisation to cancel out inverse conversions)
%%                      + cost of CSE on exponentially larger programs at program generation time (not at runtime)
%%                          / check with observable sharing stuff, e.g.
%%                                 dup v = (v , v)
%%                                 dup v = let v' = lable unique v in (v' , v')
%%
%%                    Deep and shallow embedding approach reuses the type system, some syntax of the host language for EDSL.
%%                    QDSL approach reuses the type system, and all the host language syntax.
%%                    Deep and shallow embedding uses the host language evaluator to normalise all occurences of function and product types.
%%                    QDSL approach uses a reusable normaliser to normalise all occurences of function, product types.
%%                    Two-layers
%%
%% ---------------------------------------------------------------------------
%% QDSLs: Why it is nicer to be quoted nor


\begin{document}

%% \conferenceinfo{WXYZ '05}{date, City.}
%% \copyrightyear{2005}
%% \copyrightdata{[to be supplied]}

%% \titlebanner{banner above paper title}        % These are ignored unless
%% \preprintfooter{short description of paper}   % 'preprint' option specified.

\title{QDSLs: Why its nicer to be quoted normally}

\author{Shayan Najd\inst{1}
  \and Sam Lindley\inst{1}
  \and Josef Svenningsson\inst{2}
  \and Philip Wadler\inst{1}}

\institute{The University of Edinburgh
\email{sh.najd@@ed.ac.uk, sam.lindley@@ed.ac.uk, philip.wadler@@ed.ac.uk}
\and
Chalmers University of Technology
\email{josefs@@chalmers.se}}



% \authorinfo{Shayan Najd}
%            {The University of Edinburgh}
%            {sh.najd@@ed.ac.uk}
% \authorinfo{Sam Lindley}
%            {The University of Edinburgh}
%            {sam.lindley@@ed.ac.uk}
% \authorinfo{Josef Svenningsson}
%            {Chalmers University of Technology}
%            {josefs@@chalmers.se}
% \authorinfo{Philip Wadler}
%            {The University of Edinburgh}
%            {philip.wadler@@ed.ac.uk}

\maketitle


\begin{abstract}
We describe a technique, based on quotation and normalisation of
quoted terms, for embedding into a host-language a domain-specific
language that translates into a target language. The technique was
developed by Cheney, Lindley, and Wadler (2013), who applied it to
embed queries into F\# that translate into SQL. They conjectured that
the same technique applied in other situations, and here we test that
conjecture by applying the technique to the Feldspar system of
Axelsson and others (2010), which embeds signal processing programs
into Haskell that translate into C. We validate our technique by
re-implementing Feldspar using Template Haskell quasi-quotation, and
confirm that Feldspar and our technique generate identical target code on
a range of applications including image processing, FFT, and CRC.

Feldspar, as well as similar languages such as Nicola and Hydra,
benefits from a clever combination of deep and shallow embedding that
permits programs in the embedded language to look almost---but not
quite!---identical to programs in the host language. (For instance,
arithmetic and pairing is identical in embedded and host languages,
but comparison and conditionals are not.) In contrast, our technique
makes the syntax of the embedded language identical to that of the
host language. Apart from building a normaliser for quoted terms, our
technique is simpler to apply than existing techniques; and we argue
that a single normaliser can be built once and used to embed many
domain-specific languages into a given host language.
\end{abstract}


\section{Introduction}

Do you prefer to build a domain-specific language using shallow
embedding, deep embedding, or a combination of the two?
This paper offers a new way---quoted domain-specific languages.
For brevity, we write EDSL for embedded domain specific language
(usually with a combination of deep and shallow embedding), and
QDSL for quoted domain specific language.

% advantages of QDSLs / compare to deep and shallow embeddings

% essence of QDSLs

% QDSLs are not really new / key related work

% contributions of this paper


\section{Overview}

Let's begin by considering the ``hello world'' of program generation,
the power function. Since division by zero is undefined, we arbitrarily
choose that raising zero to a negative power yields zero.
\begin{code}
power :: Int -> Float -> Float
power n x  =
  if n < 0 then
    if x == 0 then 0 else 1 / power (-n) x
  else if n == 0 then
    1
  else if even n then 
    sqr (power (n `div` 2) x)
  else
    x * power (n-1) x

sqr :: Float -> Float
sqr x  =  x * x
\end{code}
Our goal is to generate code in the programming language~C.
For example,
\begin{lstlisting}
  float main (float u) {
    if (u == 0) {
      return 0;
    } else {
      float v = u * 1;
      float w = u * (v * v);
      return 1 / (w * w);
    }
  } 
\end{lstlisting}
should result from instantiating |power| to |(-6)|.

For EDSL, we assume a type |EDSL a| to represent a term
of type |a|, and function
\begin{code}
edslC :: (EDSL a -> EDSL b) -> C
\end{code}
to generate a \texttt{main} function corresponding to its
argument.  Here is a solution to our problem using EDSL.
\begin{code}
power :: Int -> EDSL Float -> EDSL Float
power n x  =
  if n < 0 then
    x .==. 0 ? (0,  1 / power (-n) x)
  else if n == 0 then
    1
  else if even n then 
    sqr (power (n `div` 2) x)
  else
    x * power (n-1) x

sqr :: EDSL Float -> EDSL Float
sqr y  =  y * y
\end{code}
Invoking |edslC (power (-6))| generates the C code above.

Type |Float -> Float| in the original becomes
\[
|EDSL Float -> EDSL Float|
\]
in the EDSL solution, meaning that |power n| accepts a representation
of the argument and returns a representation of that argument raised
to the $n$'th power.

In EDSL, the body of the code remains almost---but not
quite!---identical to the original.  Clever encoding tricks, which we
will explain later, permit declarations, function calls, arithmetic
operations, and numbers to appear the same whether they are to be
executed at generation-time or run-time.  However, for reasons that we
will explain later, comparison and conditionals appear differently
depending on whether they are to be executed at generation-time or
run-time, using |x == 0| and |if a then b else c| for the former but
|x .==. 0| and |a ?  (b, c)| for the latter.

Assuming |x| contains a value of type |EDSL Float| denoting an object
variable |u| of type float, evaluating |power (-6) x| yields following.
\begin{code}
(u .==. 0) ? (0, 1 / (u * ((u * 1) * (u * 1))) * (u * ((u * 1) * (u * 1))))
\end{code}
Applying common-subexpression elimination, or using a technique such
as observable sharing, permits recovering the sharing structure.
\[
\begin{array}{r@@{~}||@@{~}l}
|v| & |(u * 1)|  \\
|w| & |u * (v * v)| \\
|main| &  |(u .==. 0) ? (0, 1/(w*w))|
\end{array}
\]
It is easy to generate the final C code from this structure.

By contrast, for QDSL, we assume a type |QDSL a| to represent a
quoted term of type |a|, and function
\begin{code}
qdslC :: QDSL (a -> b) -> C
\end{code}
to generate a \texttt{main} function corresponding to its
argument.  Here is a solution to our problem using QDSL.
\begin{code}
power :: Int -> Exp (Float -> Float)
power n = 
  if n < 0 then
    <|| \x -> if x == 0 then 0 else 1 / $(power (-n)) x ||>
  else if n == 0 then
    <|| \x -> 1 ||>
  else if even n then 
    <|| \x -> $(sqr) ($(power (n `div` 2)) x) ||>
  else
    <|| \x -> x * $(power (n-1)) x ||>

sqr :: Exp (Float -> Float)
sqr  =  <|| \y -> y * y ||>
\end{code}
Invoking |qdslC (power (-6))| generates the C code above.

Type |Float -> Float| in the original becomes
\[
|QDSL (Float -> Float)|
\]
in the QDSL solution, meaning that |power n| returns a quotation
of a function that accepts an argument and returns that
argument raised to the $n$'th power.

In QDSL, the body of the code changes more substantially. The typed
quasi-quoting mechanism of Template Haskell is used to indicate which
code executes at which time.  Unquoted code executes at
generation-time while quoted code executes at run-time. Quoting is
indicated by \( <|| \cdots ||> \) and unquoting by \( \$(\cdots) \).
Here, by the mechanism of quoting, without any need for tricks,
the syntax for code excecuted at both generation-time and run-time is
identical for all constructs, including comparison and conditionals.

Now evaluating |power (-6)| yields the following.
\begin{code}
<|| \x ->
     if x == 0 then 0 else
       1 / (\y -> y * y) (x * (\y -> y * y) (x * 1)) ||>
\end{code}
Normalising the term, with variables renamed
for readability, yields the following.
\begin{code}
<|| \u ->
      if u == 0 then 0 else
        let v = u * 1 in
          let w = u * (v * v) in
            1 / w * w  ||>
\end{code}
It is easy to generate the final C code from
the normalised term.

Here are some points of comparison between the two approaches.
\begin{itemize}

\item EDSL requires some term forms, such as comparison and
conditionals, to differ between the host and embedded languages.
In contrast, QDSL enables the host and embedded languages to
appear identical.

\item EDSL permits the host and embedded languages to intermingle
seamlessly. In contrast, QDSL requires syntax to separate quoted and
unquoted terms, which (depending on your point of view) may be
considered as an unnessary distraction or as drawing a useful
distinction between generation-time and run-time.  If one takes the
former view, the type-based approach to quotation found in C\# and
Scala might be preferred.

\item EDSL typically develops custom shallow and deep embeddings for
each application, although these may follow a fairly standard pattern
(as we review in Section~\ref{sec:shallow-deep}).  In contrast, QDSL
may share the same representation for quoted terms across a range of
applications; the quoted language is the host language, and does not
vary with the specific domain.

\item EDSL loses sharing, which must later be recovered be either
common subexpression elimination or applying a technique such as
observable sharing.  Common subexpression elimination can be
expensive, as we will see in the FFT example in Section~\ref{sec:fft}.
Observable sharing is less costly, but requires stepping outside a
pure functional model. In contrast, QDSL preserves sharing throughout.

\item EDSL yields the term in normalised form in this case, though
there are other situations where a normaliser is required (see
Section~\ref{sec:option}).  In contrast, QDSL yields an unwieldy term
that requires normalisation.  However, just as a single representation
of QDSL terms suffices across many applications, so does a single
normaliser---it can be built once and reused many times.

\item Once the deep embedding or the normalised quoted term is
produced, generating the domain-specific code is similar for
both approaches.
 
\end{itemize}

\section{A second example}

In the previous code, we arbitrarily chose that raising zero to a
negative power yields zero. Say that we wish to exploit the |Maybe| type
to refactor the code to separate recognising the exceptional case
(negative exponent of zero) from choosing a value for this case (zero).
We decompose |power| into two functions |power'| and |power''|, where the first
returns |Nothing| in the exceptional case, and the second maps |Nothing|
to a suitable default value.
\begin{code}
power' :: Int -> Float -> Maybe Float
power' n x  =
  if n < 0 then
    if x == 0 then Nothing else do y <- power' (-n) x; return (1 / y)
  else if n == 0 then
    return 1
  else if even n then 
    do y <- power' (n `div` 2) x; return (sqr y)
  else
    do y <- power' (n-1) x; return (x * y)

power'' :: Int -> Float -> Float
power'' n x  =  maybe 0 (\x -> x) (power' n x)

sqr :: Float -> Float
sqr x  =  x * x
\end{code}
The above uses the function
\begin{code}
maybe :: b -> (a -> b) -> Maybe a -> b
\end{code}
from the Haskell library |Data.Maybe|.
The same C code as before should result from instantiating |power''| to |(-6)|.

(In this case, the refactored function is arguably clumsier than the original,
but clearly it is desirable to support this form of refactoring in general.)

In EDSL, type |Maybe| is represented by type |Option|.
Here is the refactored code.
\begin{code}
power' :: Int -> EDSL Float -> Option (EDSL Float)
power' n x  =
  if n < 0 then
    (x .==. 0) ? (none, do y <- power' (-n) x; return (1 / y))
  else if n == 0 then
    return 1
  else if even n then 
    do y <- power' (n `div` 2) x; return (sqr y)
  else
    do y <- power' (n-1) x; return (x*y)

power'' :: Int -> EDSL Float -> EDSL Float
power'' n x  = option (\y -> y) id (power' n x)

sqr :: EDSL Float -> EDSL Float
sqr y  =  y * y
\end{code}
The above uses the functions
\begin{code}
none   :: Option a
return :: a -> Option a
(>>=)  :: Option a -> (a -> Option b) -> Option b
option :: (Syntactic a, Syntactic b) => b -> (a -> b) -> Option a -> b
\end{code}
from the EDSL library. Details of the type |Option| and the type class |Syntactic|
are explained in Section~\ref{sec:option}. A clever aspect of the representation
is that the type |Option| is declared as a monad,
so that the usual |do| notation may be used.
Invoking |edslC (power'' (-6))| generates the same C code as
the previous example.

Before the |power| function generated code in essentially
normal form, save for the need to use common subexpression elimination
or observable sharing to recover shared structure. In this case, 
the generated code is far from normal form. Rewrite rules including
the following need to be repeatedly applied.
\[
\begin{array}{l}
|fst (a, b)| \leadsto a \\
|snd (a, b)| \leadsto b \\
|fst (if c then t else e)| \leadsto |if c then fst t else fst e| \\
|snd (if c then t else e)| \leadsto |if c then snd t else snd e| \\
|if (if c then t else e) then t' else e'| \leadsto {} \\
\quad |if c then (if t then t' else e') else (if e then t' else e')| \\
|if c then (if c then t' else e') else e| \leadsto |if c then t' else e|
\end{array}
\]



\section{Shallow and deep embedding}


\section{Combining shallow and deep embedding}


\section{The subformula property}


\section{The design of QuickDSL}


\section{Empirical results}




\end{document}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
----------------------------
"Good artists copy, great artists steal." --- Picasso

Domain specific embedded languages are great: they steal the type
system of the host language.  This paper presents quoted domain
specific languages, which are greater still: they steal the type
system and syntax of the host language, and exploit a normaliser for
the quoted expressions that can be reused between many implementations
of embedded languages.
-----------------------------
-- Higher-order is the secret sauce of functional programming.
Many embedded domain specific languages, such as Feldspar, Hydra, and
Nicola, benefit from allowing programmers to exploit higher-order
features while generating efficient first-order code.
The reason this works is Gentzen's \emph{subformula property}, which gaurantees that higher-order constructs


-----------------------------
Sometimes a detour is the shortest way to one's destination.
Often, a concept can be most concisely expressed using higher-order
functions, eventhought, the final result is first-order.
Many embedded domain specific languages, such as Feldspar, Hydra, and
Nicola, benefit from allowing programmers to exploit higher-order
features while generating efficient first-order code.
They achieve this by utilizing the evaluator of the host language;
We achieve it by using a normaliser instead. Our normaliser is gauranteed to work because of the theorems depending on subformula property.
 
We present a new approach to implementing embedded domain specific languages based on quotations called QDSLs (Quoted DSLs).

 
\section{Background}

\subsection{Feldspar}

\subsection{Quotation-based DSLs}

\section{Template Feldspar}

\section{Performance Evaluation}

\section{Related Work}

\section{Conclusion}

%% \begin{abstract}
%% This is the text of the abstract.
%% \end{abstract}

%% \category{CR-number}{subcategory}{third-level}

%% \terms
%% term1, term2

%% \keys
%% keyword1, keyword2

%% \section{Introduction}

%% The text of the paper begins here.

%% \appendix
%% \section{Appendix Title}

%% This is the text of the appendix, if you need one.

%% \acks

%% Acknowledgments, if needed.

% We recommend abbrvnat bibliography style.

% \bibliographystyle{abbrvnat} \bibliography{paper}

% The bibliography should be embedded for final submission.

%% \begin{thebibliography}{}
%% \softraggedright

%% \bibitem[Smith et~al.(2009)Smith, Jones]{smith02}
%% P. Q. Smith, and X. Y. Jones. ...reference text...

%% \end{thebibliography}

\end{document}

