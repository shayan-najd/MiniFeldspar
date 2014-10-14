\documentclass[citeauthoryear]{llncs}
%\documentclass[preprint]{sigplanconf}

%include polycode.fmt
%format == = "\longeq "
%format || = "||"
%format <|| = "\openq "
%format ||> = "\closeq "
%format `div` = "\rmdiv"
%format <*> = "\mathbin{{<}\!{*}\!{>}}"
%format .==. = "\mathbin{{.}{" == "}{.}}"
%format .<.  = "\mathbin{{.}{" < "}{.}}"
%format x_0
%format Opt_R
%format some_R
%format none_R
%format opt_R
%format forall = "\forall"

% US Letter page size
%\pdfpagewidth=8.5in
%\pdfpageheight=11in

% The following \documentclass options may be useful:
%
% 10pt          To set in 10-point type instead of 9-point.
% 11pt          To set in 11-point type instead of 9-point.
% authoryear    To obtain author/year citation style instead of numeric.

%%\usepackage[round]{natbib}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
%%\usepackage{amsthm}
\usepackage{stmaryrd}
\usepackage{proof}
\usepackage{xspace}
\usepackage[pdfauthor={Shayan Najd,Sam Lindley,Josef Svenningsson,Philip Wadler}
                      ,pdftitle={QDSLs: Why its nicer to be quoted normally}
                      ,pagebackref=true,pdftex]{hyperref}
\usepackage{tabularx}
\usepackage{graphicx}
\usepackage{url}
\usepackage{color}
\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\usepackage{listings}
\lstset{language=C,identifierstyle=\ttfamily,keywordstyle=\bfseries\ttfamily}
%% \usepackage{colortbl}

%%% macros

\newcommand{\todo}[1]{{\noindent\small\color{red} \framebox{\parbox{\dimexpr\linewidth-2\fboxsep-2\fboxrule}{\textbf{TODO:} #1}}}}
%\newcommand{\todo}[1]{}

\newcommand{\longeq}{=\!=}
\newcommand{\openq}{[||||\,}
\newcommand{\closeq}{\,||||]}
\newcommand{\rmdiv}{\mathbin{\textrm{div}}}

\newcommand{\citet}[1]{\cite{#1}}
\newcommand{\citep}[1]{(\cite{#1})}

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

\begin{quotation} \flushright
Good artists copy, great artists steal. --- Picasso
\end{quotation}
\vspace{2ex}

Which shall it be, CDSL or QDSL?

Say you wish to use a domain-specific language embedded in a host
language to generate code in a target language.  One widely-used
technique combines deep and shallow embedding, which we refer to as
CDSL (Combined Domain Specific Language).  Here we introduce a second
technique based on quotation and normalisation of quoted terms, which
we refer to as QDSL (Quoted Domain Specific Language).

CDSL is great in part because it steals the type system of its host
language. Arguably, QDSL is greater because it steals the type system,
the concrete syntax, and the abstract syntax of its host language.

CDSL sometimes, but not always, avoids the need for normalisation.
QDSL depends crucially on normalisation. Each CDSL has a
different deep embedding, so when normalisation is required, a new
normaliser needs to be written for each CDSL. In contrast, all QDSLs for a
host language share the same deep embedding---namely, the abstact
syntax of the host language---so they can share a single normaliser.

Both CDSL and QDSL steal types from the host language. Sometimes, it
proves convenient to steal a type from the host even when we expect it
never to appear in target. The most common example is when we exploit
higher-order types in the host even when the target supports only
first-order types.  But other examples are plentiful; here we provide
an example of using the |Maybe| type of the host even though we don't
expect to provide that type in the target. We show how these
situations are neatly handled by an application of Gentzen's
subformula property, exploiting a result from logic in 1935 to advance
computing eight decades later.

\begin{quotation}\flushright
Perhaps we may express the essential properties of such a normal proof
by saying: it is not roundabout.
% No concepts enter into the proof other
% than those contained in its final result, and their use was therefore
% essential to the achievement of that result.
--- Gerhard Gentzen
\end{quotation}
\vspace{2ex}

The QDSL technique to building domain-specific languages was proposed
by \citet{CheneyLW13}, who used it to integrate SQL queries into F\#.
They conjectured that the technique applies more widely, and here we
test that conjecture by applying QDSL to Feldspar, an CDSL for signal
processing in Haskell that generates C \citep{FELDSPAR}. Our technique
depends on GHC Haskell typed quasi-quotations \citep{mainland-quoted}.

So far as we know, the full QDSL approach---which, crucially, includes
normalisation of quoted terms---has only been applied here and by
\citet{CheneyLW13}.  However, the other part of the QDSL
approach---viewing domain-specific languages as quoted terms---is
widely used in other systems, including F\# LINQ \citep{fsharplinq},
C\# LINQ \citep{csharplinq}, and Scala Lightweight Modular Staging
(LMS) \citep{scalalms}. In F\# LINQ quotation and anti-quotation are
explicit, as here, while in C\# LINQ and Scala LMS, quotation and
anti-quotation is controlled by type inference.

Feldspar exploits a combination of deep and shallow embedding,
a technique which we here refer to as simply CDSL. (In other contexts,
CDSL means any embedded domain-specific language, and includes
all techniques covered here.) The technique is clearly described
by \citet{SvenningssonA12}, and further refined by \citet{PerssonAS11}
and \citet{SvenningssonS13}. Essentially the same technique is also
applied in Obsidian \citep{svensson2011obsidian} and Nikola \citep{NIKOLA}.

In a single landmark paper, \citet{gentzen35} introduced the two
formulations of logic most widely used today, natural deduction and
sequent calculus, in both intuitionistic and classical variants.  (The
same paper introduced $\forall$ for universal quantification.)
Gentzen's main technical result was to establish the \emph{subformula}
property: any natural deduction proof may be put in a normal form
where all formulas it contains are subformulas of either its
hypotheses or conclusion. \citet{CheneyLW13} applied this result to
ensure queries with higher-order components always simplif to
first-order queries, easily translated to SQL.  Similarly here, our
QDSL source may refer to higher-order concepts or data types such as
|Maybe|, while we ensure that these do not appear in the generated
code.  The idea is not limited to QDSL, as
Section~\ref{sec:edsl-maybe} applies the same idea to CDSL.

The paper makes the following contributions.
\begin{itemize}
\item Section~\ref{sec:overview} introduces and compares the
CDSL and QDSL approaches, in the context of a simple example.

\item Section~\ref{sec:edsl} reviews how the CDSL approach
works in detail, in the context of Feldspar.

\item Section~\ref{sec:qdsl} describes how the QDSL approach
works in detail, reworking the examples of Section~\ref{sec:edsl}.

\item Section~\ref{sec:subformula} describes a normaliser
that ensures the subformula property while not losing sharing,
which can be applied to both call-by-need and call-by-value semantics.

\item Section~\ref{sec:empirical} presents empirical results for
Feldspar programs written and executed in both styles, showing CDSL
and QDSL achieve comparable results.

\end{itemize}
Section~\ref{sec:related} summarises related work, and
Section~\ref{sec:conclusion} concludes.

\section{Overview}

\subsection{First example}
\label{sec:first-example}

Let's begin by considering the ``hello world'' of program generation,
the power function. Since division by zero is undefined, we arbitrarily
choose that raising zero to a negative power yields zero.
\begin{code}
power :: Int -> Float -> Float
power n x =
  if n < 0 then
    if x == 0 then 0 else 1 / power (-n) x
  else if n == 0 then
    1
  else if even n then
    sqr (power (n `div` 2) x)
  else
    x * power (n-1) x

sqr    ::  Float -> Float
sqr x  =   x * x
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

For CDSL, we assume a type |Dp a| to represent a term
of type |a|, and function
\begin{code}
edslC :: (Dp a -> Dp b) -> C
\end{code}
to generate a \texttt{main} function corresponding to its
argument, where |C| is a type that represents |C| code.
Here is a solution to our problem using Dp.
\begin{code}
power :: Int -> Dp Float -> Dp Float
power n x =
  if n < 0 then
    x .==. 0 ? (0,  1 / power (-n) x)
  else if n == 0 then
    1
  else if even n then
    sqr (power (n `div` 2) x)
  else
    x * power (n-1) x

sqr    ::  Dp Float -> Dp Float
sqr y  =   y * y
\end{code}
Invoking |edslC (power (-6))| generates the C code above.

Type |Float -> Float| in the original becomes
\[
|Dp Float -> Dp Float|
\]
in the CDSL solution, meaning that |power n| accepts a representation
of the argument and returns a representation of that argument raised
to the $n$'th power.

In CDSL, the body of the code remains almost---but not
quite!---identical to the original.  Clever encoding tricks, which we
will explain later, permit declarations, function calls, arithmetic
operations, and numbers to appear the same whether they are to be
executed at generation-time or run-time.  However, for reasons that we
will explain later, comparison and conditionals appear differently
depending on whether they are to be executed at generation-time or
run-time, using |M == N| and |if L then M else N| for the former but
|M .==. N| and |L ?  (M, N)| for the latter.

Assuming |x| contains a value of type |Dp Float| denoting an object
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

By contrast, for QDSL, we assume a type |Qt a| to represent a
quoted term of type |a|, and function
\begin{code}
qdslC :: Qt (a -> b) -> C
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

sqr  ::  Exp (Float -> Float)
sqr  =   <|| \y -> y * y ||>
\end{code}
Invoking |qdslC (power (-6))| generates the C code above.

Type |Float -> Float| in the original becomes
\[
|Qt (Float -> Float)|
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
<|| \x ->  if x == 0 then 0 else
             1 / (\y -> y * y) (x * (\y -> y * y) (x * 1)) ||>
\end{code}
Normalising the term, with variables renamed
for readability, yields the following.
\begin{code}
<|| \u ->  if u == 0 then 0 else
             let v = u * 1 in
             let w = u * (v * v) in
             1 / w * w  ||>
\end{code}
It is easy to generate the final C code from
the normalised term.

Here are some points of comparison between the two approaches.
\begin{itemize}

\item CDSL requires some term forms, such as comparison and
conditionals, to differ between the host and embedded languages.
In contrast, QDSL enables the host and embedded languages to
appear identical.

\item CDSL permits the host and embedded languages to intermingle
seamlessly. In contrast, QDSL requires syntax to separate quoted and
unquoted terms, which (depending on your point of view) may be
considered as an unnessary distraction or as drawing a useful
distinction between generation-time and run-time.  If one takes the
former view, the type-based approach to quotation found in C\# and
Scala might be preferred.

\item CDSL typically develops custom shallow and deep embeddings for
each application, although these may follow a fairly standard pattern
(as we review in Section~\ref{sec:shallow-deep}).  In contrast, QDSL
may share the same representation for quoted terms across a range of
applications; the quoted language is the host language, and does not
vary with the specific domain.

\item CDSL loses sharing, which must later be recovered be either
common subexpression elimination or applying a technique such as
observable sharing.  Common subexpression elimination can be
expensive, as we will see in the FFT example in Section~\ref{sec:fft}.
Observable sharing is less costly, but requires stepping outside a
pure functional model. In contrast, QDSL preserves sharing throughout.

\item CDSL yields the term in normalised form in this case, though
there are other situations where a normaliser is required (see
Section~\ref{sec:second-example}).  In contrast, QDSL yields an unwieldy term
that requires normalisation.  However, just as a single representation
of QDSL terms suffices across many applications, so does a single
normaliser---it can be built once and reused many times.

\item Once the deep embedding or the normalised quoted term is
produced, generating the domain-specific code is similar for
both approaches.

\end{itemize}

\subsection{Second example}
\label{sec:second-example}

In the previous code, we arbitrarily chose that raising zero to a
negative power yields zero. Say that we wish to exploit the |Maybe| type
to refactor the code, separating identifying the exceptional case
(negative exponent of zero) from choosing a value for this case (zero).
We decompose |power| into two functions |power'| and |power''|, where the first
returns |Nothing| in the exceptional case, and the second maps |Nothing|
to a suitable default value.
\begin{code}
power' ::  Int -> Float -> Maybe Float
power' n x =
  if n < 0 then
    if x == 0 then Nothing else do y <- power' (-n) x; return (1 / y)
  else if n == 0 then
    return 1
  else if even n then
    do y <- power' (n `div` 2) x; return (sqr y)
  else
    do y <- power' (n-1) x; return (x * y)

power''      ::  Int -> Float -> Float
power'' n x  =   maybe 0 (\x -> x) (power' n x)
\end{code}
Here |sqr| is as before. The above uses
\begin{code}
data Maybe a = Nothing | Just a
return  ::  a -> Maybe a
(>>=)   ::  Maybe a -> (a -> Maybe b) -> Maybe b
maybe   ::  b -> (a -> b) -> Maybe a -> b
\end{code}
from the Haskell prelude. Type |Maybe| is declared as a monad, enabling the |do| notation,
which translates into |(>>=)|.
The same C code as before should result from instantiating |power''| to |(-6)|.

(In this case, the refactored function is arguably clumsier than the original,
but clearly it is desirable to support this form of refactoring in general.)

In CDSL, type |Maybe| is represented by type |Option|.
Here is the refactored code.
\begin{code}
power' :: Int -> Dp Float -> Option (Dp Float)
power' n x  =
  if n < 0 then
    (x .==. 0) ? (none, do y <- power' (-n) x; return (1 / y))
  else if n == 0 then
    return 1
  else if even n then
    do y <- power' (n `div` 2) x; return (sqr y)
  else
    do y <- power' (n-1) x; return (x*y)

power''      ::  Int -> Dp Float -> Dp Float
power'' n x  =   option 0 (\y -> y) (power' n x)
\end{code}
Here |sqr| is as before. The above uses the functions
\begin{code}
none   	::  Option a
return 	::  a -> Option a
(>>=)  	::  Option a -> (a -> Option b) -> Option b
option 	::  (Syntactic a, Syntactic b) => b -> (a -> b) -> Option a -> b
\end{code}
from the CDSL library. Details of the type |Option| and the type class |Syntactic|
are explained in Section~\ref{sec:option}.
Type |Option| is declared as a monad, enabling the |do| notation,
which translates into |(>>=)|.
Invoking |edslC (power'' (-6))| generates the same C code as
the previous example.

In order to be easily represented in C, type |Option a| is represented as a pair
consisting of a boolean and the representation of the type |a|; in the case that
corresponds to |Nothing|, the boolean is false and a default value of type |a|
is provided.  The CDSL term generated by evaluating |power (-6) 0| is large and
unscrutable:
\begin{code}
(((fst ((x == (0.0)) ? ((((False) ? ((True), (False))), ((False) ?  (undef,
undef))), ((True), ((1.0) / ((x * ((x * (1.0)) * (x * (1.0)))) * (x * ((x *
(1.0)) * (x * (1.0)))))))))) ? ((True), (False))) ?  (((fst ((x == (0.0)) ?
((((False) ? ((True), (False))), ((False) ?  (undef, undef))), ((True), ((1.0) /
((x * ((x * (1.0)) * (x * (1.0)))) * (x * ((x * (1.0)) * (x * (1.0)))))))))) ?
((snd ((x == (0.0)) ?  ((((False) ? ((True), (False))), ((False) ? (undef,
undef))), ((True), ((1.0) / ((x * ((x * (1.0)) * (x * (1.0)))) * (x * ((x *
(1.0)) * (x * (1.0)))))))))), undef)), (0.0)))
\end{code}
(Details of why this is the term generated will become clearer after
Section~\ref{sec:option}.)  Before, evaluating |power| yielded an CDSL term
essentially in normal form, save for the need to use common subexpression
elimination or observable sharing to recover shared structure.  However, this is
not the case here. Rewrite rules including the following need to be repeatedly
applied.

\todo{Fix typesetting of |M[L:=P]|.}
\begin{eqnarray*}
|fst (M, N)| &\leadsto& M \\
|snd (M, N)| &\leadsto& N \\
|fst (L ? (M, N))| &\leadsto& |L ? (fst M, fst N)| \\
|snd (L ? (M, N))| &\leadsto& |L ? (snd M, snd N)| \\
|True ? (M,N)| &\leadsto& |M| \\
|False ? (M,N)| &\leadsto& |N| \\
|(L ? (M, N)) ? (P,Q)| &\leadsto& |L ? (M ? (P,Q)) ? (N ? (P,Q))| \\
|L ? (M, N)| &\leadsto& |L ? (M[L:=True], N[L:=False])|
\end{eqnarray*}
Here |L|,|M|,|N|,|P|,|Q| range over |Dp| terms, and
|M[L:=P]| stands for |M| with each occurence of |L| replaced by |P|.
After applying these rules, common subexpression
elimination yields the same structure as in the previous subsection,
from which the same C code is generated.

Hence, an advantages of the CDSL approach---that it generates
terms essentially in normal form---turns out to be restricted
to a limited set of types, including functions and products,
but excluding sums. If one wishes to deal with sum types,
separate normalisation is required. This is one reason
why we do not consider normalisation as required by QDSL
to be particularly onerous.

In QDSL, the type |Maybe| can be represented by its
run-time equivalent, which we also call |Maybe|.
Here is the refactored code.
\begin{code}
power' :: Int -> Qt (Float -> Maybe Float)
power' n =
  if n < 0 then
    <|| \x ->  if x == 0 then Nothing else
                 do y <- $(power' (-n) x); return (1 / y) ||>
  else if n == 0 then
    <|| \x -> return 1 ||>
  else if even n then
    <|| \x -> do y <- $(power' (n `div` 2) x); return $(sqr y) ||>
  else
    <|| \x -> do y <- $(power' (n-1) x); return (x * y) ||>

power''      ::  Int -> Qt (Float -> Float)
power'' n x  =   <|| maybe 0 (\x -> x) $(power' n x) ||>
\end{code}
Here |sqr| is as before,
and |Nothing|, |return|, |(>>=)|, and |maybe| are typed as before,
and provided for use in quoted terms by the QDSL library.

Evaluating |Qt (powerQ'' (-6))| yields a term of similar complexity
to the term yielded by the CDSL. Normalisation by the rules discussed
in Section~\ref{sec:normalise} reduces the term to the same form
as before, which in turn generates the same C as before.

Here are some further points of comparison between the two approaches.
\begin{itemize}

\item Both CDSL and QDSL can exploit notational conveniences in the
host language. The example here exploits Haskell |do| notation; the
embedding SQL in F\# by \citet{CheneyLW13} expoited F\# sequence
notation. For the CDSL, exploiting |do| notation just requires
instantiating |return| and |(>>=)| correctly. For the QDSL, it is
also necessary for the normaliser to recognise and expand
|do| notation and to substitute appropriate instances of
|return| and |(>>=)|.

\item As this example shows, sometimes both CDSLs and QDSLs
may require normalisation. Each CDSL usually has a distinct
deep representation and so requires a distinct normaliser.
In contrast, all QDSLs can share the representation of the
quoted host language, and so can share a normaliser.

\end{itemize}

\section{MiniFeldspar as a CDSL}
\label{sec:edsl}

%include section3.lhs

\section{MiniFeldspar as a QDSL}
\label{sec:qdsl}


\section{The subformula property}
\label{sec:subformula}


\input{formalism}

\section{Empirical results}
\label{sec:empirical}


%\section{The design of Haskell DSL}
%\label{sec:tool}

\section{Related work}
\label{sec:related}

Domain specific languages are becoming increasingly popular as a way
to deal with software complexity. Yet, they have a long and rich
history \citep{Bentley:1986:PPL:6424.315691}.

In this paper we have, like many other DSL writers, used Haskell as it
has proven to be very suitable for \emph{embedding} domain specific
languages.  Examples include \citep{reid1999prototyping,
hudak1997domain, bjesse1998lava, NIKOLA, svensson2011obsidian,
FELDSPAR}.

In this paper we have specifically built on the technique of combining
deep and shallow embeddings \citep{SvenningssonA12} and
contrasted it with our new QDSL technique. Languages which have used
this technique include Feldspar \citep{FELDSPAR}, Obsidian
\citep{svensson2011obsidian} and Meta-Repa \citep{ankner2013edsl}.

The loss of sharing when implementing embedded DSLs was identified by
\citet{claessen1999observable}. They proposed to introduce a little
bit of impurity in Haskell, referred to as \emph{observable sharing}
to be able to recover the loss of sharing. Later, \citet{gill2009type}
proposed a somewhat safer way of recover sharing, though still relying
on impurity.

\section{Conclusion}
\label{sec:conclude}

\paragraph*{Acknowledgements}
This work was funded by EPSRC Programme Grant EP/K034413/1.


\bibliographystyle{plainnat}
\bibliography{paper}


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
