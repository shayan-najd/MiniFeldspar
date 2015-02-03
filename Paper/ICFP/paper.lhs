\documentclass{llncs}

%include polycode.fmt
%include forall.fmt

%format == = "\longeq "
%format || = "||"
%format `div` = "\rmdiv"
%format * = "\times"
%format <*> = "\mathbin{{<}\!{*}\!{>}}"
%format .==. = "\mathbin{{.}{" == "}{.}}"
%format .<.  = "\mathbin{{.}{" < "}{.}}"
%format x_0 = x
%format Opt_R = Opt'
%format some_R = some'
%format none_R = none'
%format opt_R = opt'
%format option_R = option'
%format Opt_0
%format some_0
%format none_0
%format opt_0
%format option_0
%format power_Dp = power
%format power_Dp' = power'
%format power_Dp'' = power''
%format sqr_Dp = sqr
%format power_Qt = power
%format power_Qt' = power'
%format power_Qt'' = power''
%format sqr_Qt = sqr
%format ^ = " "

% US Letter page size
%\pdfpagewidth=8.5in
%\pdfpageheight=11in

% The following \documentclass options may be useful:
%
% 10pt          To set in 10-point type instead of 9-point.
% 11pt          To set in 11-point type instead of 9-point.
% authoryear    To obtain author/year citation style instead of numeric.

\usepackage[round]{natbib}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{stmaryrd}
\usepackage{proof}
\usepackage{xspace}
\usepackage[pdfauthor={Sam Lindley,Shayan Najd,Josef Svenningsson,Philip Wadler}
                      ,pdftitle={Everything old is new again: Quoted domain specific languages}
                      ,pagebackref=true,pdftex,backref=none]{hyperref}
\usepackage{tabularx}
\usepackage{graphicx}
\usepackage{url}
\usepackage{color}
\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\usepackage{listings}
\lstset{language=C,identifierstyle=\ttfamily,keywordstyle=\bfseries\ttfamily}
%%\usepackage{colortbl}
%%\usepackage{amsthm}

%%% macros

\newcommand{\todo}[1]{{\noindent\small\color{red} \framebox{\parbox{\dimexpr\linewidth-2\fboxsep-2\fboxrule}{\textbf{TODO:} #1}}}}
%\newcommand{\todo}[1]{}

\newcommand{\longeq}{=\!=}
\newcommand{\openq}{[||||\,}
\newcommand{\closeq}{\,||||]}
\newcommand{\rmdiv}{\mathbin{\textrm{div}}}

\makeatletter
\renewcommand\bibsection%
{
  \section{\refname
    \@@mkboth{\MakeUppercase{\refname}}{\MakeUppercase{\refname}}}
}
\makeatother



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\title{Everything old is new again: Quoted domain specific languages}

\author{Sam Lindley\inst{1}
  \and Shayan Najd\inst{1}
  \and Josef Svenningsson\inst{2}
  \and Philip Wadler\inst{1}}

\institute{The University of Edinburgh\\
  \email{sh.najd@@ed.ac.uk, sam.lindley@@ed.ac.uk, philip.wadler@@ed.ac.uk}
  \and
  Chalmers University of Technology\\
  \email{josefs@@chalmers.se}}

\maketitle

\begin{abstract}

Fashions come, go, return. We describes a new
approach to domain specific languages, called QDSL, that resurrects
two old ideas: quoted terms for domain specific languages, from
McCarthy's Lisp of 1960, and the subformula property, from Gentzen's
natural deduction of 1935.  Quoted terms allow the domain specific
language to share the syntax and type system of the host
language. Normalising quoted terms ensures the subformula property,
which provides strong guarantees, e.g., that one can use higher-order
or nested code in the source while guaranteeing first-order or flat
code in the target, or using types guide loop fusion.  We give three
examples of QDSL: QFeldspar (a variant of Feldsar), P-LINQ for F\#,
and some uses of Scala LMS; and we provide a comparison between QDSL
and EDSL (embedded DSL).

\end{abstract}

% \begin{abstract}
% 
% Domain Specific Languages (DSLs) are widely used.
% This paper introduces QDSL (Quoted DSL) 
% to consolidate a collection of concepts emerging as an approach
% to designing DSLs.
% The key concepts are
% 
% \begin{itemize}
% 
% \item Terms of the DSL are quoted, or are assembled using quotation
%       and antiquotation. While EDSLs steal the type system of the
%       embedding language and often closely resemble its syntax,
%       QDSLs steal both the type system and the syntax.
%       Quotation may be either traditional or type-based.
% 
% \item Normalisation guarantees types satisfy Gentzen's subformula property.
%       In particular, this allows one to write higher-order terms while
%       guaranteeing to generate first-order code, to allow nested intermediate
%       terms while generating code that operates on flat data, or to guarantee
%       fusion of iterations over collections in generated code.
% 
% \end{itemize}
% We contrast QDSL with the traditional EDSL (Embedded DSL) approach,
% and we offer three examples of the notion of QDSL.
% \begin{itemize}
% 
% \item We port Feldspar, and EDSL in Haskell, to a QDSL.
% 
% \item P-LINQ, an embedding of SQL in F# described by Cheney et al (2013).
% 
% \item Lightweight Modular Staging (LMS) in Scala, as used in Delite and other systems.
% 
% \end{itemize}
% \end{abstract}

% Fix: first sentence of abstract is dull
% Fix: add sentence to abstract about reusing an idea
%      four-fifths of a century old in a new context


\section{Introduction}
\label{sec:introduction}

% "The difficulty lies not so much in developing new ideas as in escaping from old ones."
% - John Maynard Keynes

% I can't understand why people are frightened of new ideas. I'm frightened of the old ones.
% - John Cage

\begin{quotation} \flushright
Don't throw the past away \\
You might need it some rainy day \\
Dreams can come true again \\
When everything old is new again \\
-- Peter Allen and Carole Sager
\end{quotation}
\vspace{2ex}

Implementing domain-specific languages (DSLs) via quotation is one of
the oldest ideas in computing, going back at least to macros in Lisp.
Today, a more fashionable technique is Embdedded DSLs (EDSLs), which
may use shallow embedding, deep embedding, or a combination of the
two. Our goal in this paper is to reinvigorate the idea of building
DSLs via quotation, by introducing a new approach that depends
crucially on normalising the quoted term, which we dub Quoted DSLs
(QDSLs).

\begin{quotation} \flushright
Imitation is the sincerest of flattery. --- Charles Caleb Colton
\end{quotation}
\vspace{2ex}

\citet{CheneyLW13} describes a DSL for language-integrated query in F\#
that translates into SQL. The approach relies on the key features of 
QDSL---quotation, normalisation of quoted terms, and the subformula property---and
the paper conjectures that these may be useful in other settings.

Here we test that conjecture by reimplementing the EDSL Feldspar
\citet{FELDSPAR} as a QDSL. We describe the key features of
the design, and show that the performance of the two versions is
comparable. We argue that, from the user's point of view, the QDSL
approach may sometimes offer a considerable simplification as
compared to the EDSL approach. To back up that claim, we describe
the EDSL approach to Feldspar for purposes of comparison.
The QDSL description occupies TODO:M pages, while
the EDSL decription requires TODO:N pages.

We also claim that Lightweight Modular in Staging (LMS) as developed
by Scala has much in common with QDSL: it often uses a type-based form
of quotation, and some DSLs implemented with LMS exploit normalisation
of quoted terms using smart constructors, and we suggest that such
DSLs may benefit from the subformula property.  LMS is a flexible
library offering a range of approaches to building DSLs, only some of
which make use of type-based quotation or normalisation via
smart-constructors; so our claim is that some LMS implementations use
QDSL techniques, not that QDSL subsumes LMS.

TODO: work out which specific LMS DSLs to cite. Scala-to-SQL is one,
what are the others?

\begin{quotation}\flushright
Perhaps we may express the essential properties of such a normal proof
by saying: it is not roundabout.
--- Gerhard Gentzen
\end{quotation}
\vspace{2ex}

Our approach exploits the fact that normalised terms satisfy the
subformula property of \citet{Gentzen35}.
The subformula property provides users of the
DSL with useful guarantees, such as the following:
\begin{itemize}

\item write higher-order terms while guaranteeing to generate
first-order code;

\item write a sequence of loops over arrays while guaranteeing to
generate code that fuses those loops;

\item write nested intermediate terms while guaranteeing to generate
code that operates on flat data.

\end{itemize}
We thus give modern application to a theorem four-fifths of a century old.

The subformula property holds only for terms in normal form.  Previous
work, such as \citet{CheneyLW13} uses a call-by-name normalisation
algorithm that performs full beta-reduction, which may cause
computations to be repeated.  Here we present call-by-value and
call-by-need normalisation algorithms, which guarantee to preserve
sharing of computations.

\begin{quotation} \flushright
Good artists copy, great artists steal. --- Picasso
\end{quotation}
\vspace{2ex}

EDSL is great in part because it steals the type system of its host
language. Arguably, QDSL is greater because it steals the type system,
the syntax, and the normalisation rules of its host language.

In theory, an EDSL should also steal the syntax of its host language,
but in practice this is often only
partially the case. For instance, an EDSL such as Feldspar or Nicola,
when embedded in Haskell, can use the overloading of Haskell so that
arithmetic operations in both languages appear identical, but the same
is not true of comparison or conditionals. In QDSL, of necessity the
syntax of the host and embedded languages must be identical. For
instance, this paper presents a QDSL variant of Feldspar, again in
Haskell, where arithmetic, comparison, and conditionals are all
represented by quoted terms of the host, hence necessarily identical.

In theory, an EDSL also steals the normalisation rules of its host
language, by using evaluation in the host to normalise terms of the
target. In Section~\ref{sec:qdsl-vs-edsl} we give two examples comparing
our QDSL and EDSL versions of Feldspar. In the first of these, it is
indeed the case that the EDSL achieves by evaluation of host terms
what the QDSL achieves by normalisation of quoted terms.  However, in
the second, the EDSL must perform some normalisation of the deep
embedding corresponding to what the QDSL achieves by normalisation of
quoted terms.

\begin{quotation}
\flushright
Try to give all of the information to help others to judge the value
of your contribution; not just the information that leads to judgment
in one particular direction or another. --- Richard Feynman
\end{quotation}
\vspace{2ex}

The subformula property depends on normalisation, but normalisation
may lead to an exponential blowup in the size of the normalised
code. In particular, this occurs when there are nested conditional or
case statements. We explain how the QDSL technique can offer the user
control over where normalisation does and does not occur, while still
maintaining the subformula property.

Some researchers contend that an essential property of an
embedded DSL which generates target code is that every term
that is type-correct should successfully generate code in
the target language. Neither the P-LINK of \citet{CheneyLW13}
nor the QFeldspar of this paper satisfy this property.
It is possible to ensure the property with additional preprocessing;
we clarify the tradeoff between ease of
implementation and ensuring safe compilation to target at
compile-time rather than run-time.

\begin{quotation}
\flushright
TODO: some quotation suitable for contributions (or summary)
\end{quotation}
\vspace{2ex}

The contributions of this paper are:
\begin{itemize}

\item To suggest the general value of an approach to building DSLs
based on quotation, normalisation of quoted terms, and the subformula
property, and to name this approach QDSL. (Section~\ref{sec:introduction}.)

\item To present the design of a QDSL implementation of Feldspar, and
show its implementation length and performance is comparable to an
EDSL implementation of Feldspar. (Section~\ref{sec:qfeldspar}.)

\item To explain the role of the subformula property in formulating
DSLs, and to describe a normalisation algorithm suitable for
call-by-value or call-by-need, which ensures the subformula property
while not losing sharing of quoted terms.
(Section~\ref{sec:subformula}.)

\item To review the F\# implementation of language-integrated query
\citep{CheneyLW13} and the Scala LMS implementations of query
and [TODO: what else?], and argue that these are instances of QDSL.
(Section~\ref{sec:other-qdsls}.)

\item To argue that, from the user's point of view, the QDSL
implementation of Feldspar is conceptually easier to understand than
the EDSL implementation of Feldspar, by a detailed comparison of
the user interface of the two implementations (Section~\ref{sec:qdsl-vs-edsl}.)

\end{itemize}
Section~\ref{sec:related} describes related work, and
Section~\ref{sec:conclusion} concludes.

\section{A QDSL variant of Feldspar}
\label{sec:qfeldspar}

Feldspar is an EDSL for writing signal-processing software, that
generates code in C \citep{FELDSPAR}. We present a variant,
QFeldspar, that follows the structure of the previous design closely,
but using the methods of QDSL rather than EDSL. We make a detailed
comparison of the QDSL and EDSL designs in Section~\ref{qdsl-vs-edsl}.

\subsection{Design}
\label{sec:qfeldspar-design}

Our goal is to translate a quoted term to C code, so we also assume a
type |Cd| that represents code in C. The top-level function of
QFeldspar has the type:
\begin{spec}
qdsl :: (FO a , FO b) => Qt (a -> b) -> Cd
\end{spec}
which generates a \texttt{main} function that takes an argument
of type |a| and returns a result of type |c|. Feldspar is designed
to generate first-order code; while Feldspar programs often use
higher-order functions, the generated C code should only use
first-order data. Hence the argument type |a| and result type |b|
of the main function must be first-order, which is indicated by
the type-class restrictions |FO a| and |FO b|.

[TODO: explain the following.]
\begin{spec}
instance FO Int
instance FO Float
instance FO a => FO (Arr a)
\end{spec}
Note that we do not have instances for |Maybe a| or |Vec a|,
which prohibits creating C code that operates on these types.

[TODO: The following is adapted from Section 2 of ESOP submission.
Might be better to replace |power| by an example that demonstrates
\texttt{while} loops and specialisation, and leave |power| until
Section~\ref{sec:qdsl-vs-edsl}.]

Let's begin by considering the ``hello world'' of program generation,
the power function, raising a float to an arbitrary integer.  We
assume a type |Qt a| to represent a term of type |a|, its
\emph{quoted} representation. Since division by zero is undefined, we
arbitrarily choose that raising zero to a negative power yields zero.
Here is the power function represented using QDSL:
\begin{code}
power :: Int -> Qt (Float -> Float)
power n =
  if n < 0 then
    [|| \x -> if x == 0 then 0 else 1 / ($$(power (-n)) x) ||]
  else if n == 0 then
    [|| \x -> 1 ||]
  else if even n then
    [|| \x -> $$sqr ($$(power (n `div` 2)) x) ||]
  else
    [|| \x -> x * ($$(power (n-1)) x) ||]

sqr  ::  Qt (Float -> Float)
sqr  =   [|| \y -> y * y ||]
\end{code}
Invoking |qdsl (power (-6))| generates the following C code.
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
The typed quasi-quoting mechanism of Template Haskell is used to
indicate which code executes at which time.  Unquoted code executes at
generation-time while quoted code executes at run-time. Quoting is
indicated by \( [||||\cdots ||||] \) and unquoting by \( \$\$(\cdots)
\).

Evaluating |power (-6)| yields the following:
\begin{spec}
[|| \x ->  if x == 0 then 0 else
      1 / (\x ->  (\y -> y * y)
          ((\x -> (x * ((\x -> (\y -> y * y)
           ((\x -> (x * ((\x -> 1) x))) x)) x))) x)) x ||]
\end{spec}
Normalising using the technique of Section~\ref{sec:subformula},
with variables renamed for readability, yields the following:
\begin{spec}
[|| \u ->  if u == 0 then 0 else
             let v = u * 1 in
             let w = u * (v * v) in
             1 / (w * w)  ||]
\end{spec}
It is easy to generate the final C code from
the normalised term.

\subsection{Subformula property}

[TODO: need an informal summary of the subformula property here,
to explain how it is used in the rest of this section. The
formal development comes in Section~\ref{sec:subformula}.]

\subsection{While}

\begin{spec}
while :: (FO s) => Qt ((s -> Bool) -> (s -> s) -> (s -> s))
\end{spec}

[TODO: Observe that the |FO s| restriction in the definition of |while| is
crucial. Without it, the subformula property could not guarantee to eliminate
types such as |Vec a| or |Maybe a|. The reason we can eliminate these types
is because they are not legal as instantiations of |s| in the definition above.

Example.
\begin{code}
for :: (FO s) => Qt (Int -> s -> (Int -> s -> s) -> s)
for =  [|| \n x_0 b -> snd (while (\(i,x) -> i < n) (\(i,x) -> (i+1 , b i x)) (0, x_0)) ||]
\end{code}

[TODO: Here is Fibonacci, but better to have an example involving specialisation.]

\begin{code}
fib :: Qt (Int -> Int)
fib =  [|| \n -> $$for n (\(a,b) -> (b,a+b)) (0,1) |]]
\end{code}
                        
\subsection{Arrays}

Two types, |Arr| for manifest arrays and |Vec| for ``pull arrays'' guaranteed
to be eliminated by fusion.
\begin{spec}
type Arr a  =  Array Int a
data Vec a  =  Vec Int (Int -> a)
\end{spec}
Recall that if |FO a| then |FO (Arr a)|, but not |FO (Vec a)|.

We assume the following primitive operations.
\begin{spec}
arr      ::  FO a => Int -> (Int -> a) -> Arr a
arrLen   ::  FO a => Arr a -> Int
arrIx    ::  FO a => Arr a -> Int -> a
\end{spec}

\begin{code}
toArr        ::  Qt (Vec a -> Arr a)
toArr        =   [|| \(Vec n g) -> arr n (\ x -> g x) ||]

fromArr      ::  Qt (Arr a -> Vec a)
fromArr      =   [|| \a -> Vec (arrLen a) (\i -> arrIx a i) ||]
\end{code}

\begin{code}
zipWithVec   ::  Qt ((a -> b -> c) -> Vec a -> Vec b -> Vec c)
zipWithVec   =   [||  \f (Vec m g) (Vec n h) ->
                        Vec ($$minim m n) (\i -> f (g i) (h i)) ||]

sumVec       ::  (FO a, Num a) => Qt (Vec a -> a)
sumVec       =   [|| \(Vec n g) -> $$for n 0 (\i x -> x + g i) ||]

scalarProd   ::  (FO a, Num a) => Qt (Vec a -> Vec a -> a)
scalarProd   =   [|| \u v -> $$sumVec ($$zipWithVec (*) u v) ||]

norm         ::  Qt (Arr Float -> Float)
norm         =   [|| \ v -> let w = fromArr v in $$scalarProd v v ||]
\end{code}

Invoking |qdsl norm| produces the following C code.
\begin{lstlisting}
// [TODO: give translation to C.]
\end{lstlisting}

\subsection{Implementation}
\label{seq:qfeldspar-implementation}

[TODO: Section 6 of ESOP submission]

\section{The subformula property}
\label{sec:subformula}

[TODO: Section 5 of ESOP submission]

[TODO: explain how the FO restriction on |while| works in conjunction
with the subformula property.]

[TODO: explain how the subformula property interacts with |fix| as
a constant in the language.]

[TODO: explain how including an uninterpreted constant |id| allows
us to disable reduction whenever this is desirable.]

\section{Other examples of QDSLs}
\label{sec:other-qdsls}

\subsection{F\# P-LINQ}
\label{sec:linq}

\subsection{Scala LMS}
\label{sec:lms}

\section{A comparison of QDSL and EDSL}
\label{sec:qdsl-vs-edsl}

[TODO: Sections 2 and 3 of ESOP submission]

\section{Related work}
\label{sec:related}

[TODO: Section 7 of ESOP submission]

[TODO: Citations for quotation, macros, early DSLs in Lisp]

\section{Conclusion}
\label{sec:conclusion}

[TODO: Section 8 of ESOP submission.]

\bibliographystyle{plainnat}
\bibliography{paper}

\end{document}







\begin{quotation} \flushright
Good artists copy, great artists steal. --- Picasso
\end{quotation}
\vspace{2ex}

Which shall it be, CDSL or QDSL?

Say you wish to use a domain-specific language embedded in a host
language to generate code in a target language.  One widely-used
technique combines deep and shallow embedding, which we dub
CDSL (Combined Domain Specific Language).  Here we introduce a second
technique based on quotation and normalisation of quoted terms, which
dub QDSL (Quoted Domain Specific Language).

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
--- Gerhard Gentzen
\end{quotation}
\vspace{2ex}

The QDSL technique (though not the name) was first proposed
by \citet{CheneyLW13}, who used it to integrate SQL queries into F\#.
They conjectured that the technique applies more widely, and here we
test that conjecture by applying QDSL to Feldspar, a DSL for signal
processing in Haskell that generates C \citep{FELDSPAR}. Our technique
depends on GHC Haskell typed quasi-quotations \citep{mainland-quoted,metahaskell}.

One key aspect of QDSL---normalisation
of quoted terms---appears only in \citet{CheneyLW13} and here.
However, another aspect of QDSL---viewing domain-specific
languages as quoted terms---is widely used in other systems, including
Lisp macros, F\# and C\# LINQ \citep{fsharplinq, csharplinq}, and
Scala Lightweight Modular Staging (LMS) \citep{scalalms}. In F\# LINQ
quotation and anti-quotation are explicit, as here, while in C\# LINQ
and Scala LMS quotation and anti-quotation are controlled by type
inference.

Feldspar exploits a combination of deep and shallow embedding, here
dubbed CDSL.  The technique is clearly described by
\citet{SvenningssonA12}, and further refined by \citet{PerssonAS11}.
Essentially the same technique is also
applied in Obsidian \citep{svensson2011obsidian} and Nikola
\citep{NIKOLA}.

In a landmark paper, \citet{gentzen35} introduced the two
formulations of logic most widely used today, natural deduction and
sequent calculus.
% in both intuitionistic and classical variants.
% (The same paper introduced $\forall$ for universal quantification.)
Gentzen's main technical result was to establish the \emph{subformula}
property: any proof may be put in a normal form
where all formulas it contains are subformulas of its
hypotheses or conclusion. \citet{CheneyLW13} applied this result to
ensure queries with higher-order components always simplify to
first-order queries, easily translated to SQL.  Similarly here, our
QDSL source may refer to higher-order concepts or data types such as
|Maybe|, while we ensure that these do not appear in the generated
code.  The idea is not limited to QDSL, and the conclusion sketches
how to apply the same idea to CDSL.

%%
We make the following contributions.
\begin{itemize}
  \item We introduce the names CDSL and QDSL, and provide a
        concise description and comparison of the two techniques.
  \item We highlight the benefits of normalisation and the
        subformula property.
  \item We introduce a collection of reduction rules for
        normalising terms that enforces the subformula property
        while ensuring sharing is preserved. The rules adapt to
        both call-by-need and call-by-value.
  \item We empirically evaluate CDSL and QDSL implementations of Feldspar
        on five benchmarks.
\end{itemize}
%%
%
%The paper is organised as follows.
Section~\ref{sec:overview} introduces and compares the
CDSL and QDSL approaches.
Section~\ref{sec:cdsl} reviews how CDSL
works in detail, in the context of Feldspar.
Section~\ref{sec:qdsl} describes how QDSL
works in detail, reworking the examples of Section~\ref{sec:cdsl}.
Section~\ref{sec:subformula} describes our normalisation algorithm.
Section~\ref{sec:evaluation} describes our implementation and
empirical evaluation.
Section~\ref{sec:related} summarises related work.
Section~\ref{sec:conclusion} concludes.

% \todo{Explain termination of normaliser and fix}

\section{Overview}
\label{sec:overview}

%include Section2.lhs

\section{MiniFeldspar as a CDSL}
\label{sec:cdsl}

%include Section3.lhs

\section{MiniFeldspar as a QDSL}
\label{sec:qdsl}

%include Section4.lhs

\section{The subformula property}
\label{sec:subformula}

\input{formalism}

\section{Implementation and evaluation}
\label{sec:evaluation}

The transformer from |Qt| to |Dp| performs the following steps.
\begin{itemize}
\item It expands identifiers connected with the types |(,)|, |Maybe|
  and |Vec|.
%%    \begin{itemize}
%%    \item For |(,)|, identifiers |fst| and |snd|.
%%    \item For |Maybe|, identifiers |return|, |(>>=)|, and |maybe|.
%%    \item For |Vec|, there are no relevant identifiers.
%%    \end{itemize}
\item It normalises the term to ensure the subformula property.
  Normalisation includes the special-purpose rules for |Maybe| and |Vec| given in
  Section~\ref{sec:qdsl} and the general-purpose rules of Section~\ref{sec:subformula}.
\item It traverses the term, converting |Qt| to |Dp|.
  It checks that only permitted primitives appear in |Qt|, and translates
  these to their corresponding representation in |Dp|. Permitted primitives include:
  |(==)|, |(<)|, |(+)|, |(*)|, and similar, plus
  |while|, |arr|, |arrLen|, and |arrIx|.
%%    \begin{itemize}
%%    \item |(==)| and |(<)|, treated as operations on integers.
%%    \item |(+)|, |(*)|, and other operations on integer and float.
%%    \item |while|, |arr|, |arrLen|, |arrIx|, which translate to
%%      |While|, |Arr|, |ArrLen|, and |ArrIx|.
%%    \end{itemize}
\end{itemize}

An unfortunate feature of typed quasiquotation in GHC is that the
implementation discards all type information when creating the
representation of a term.  Type |Qt a| is equivalent to
|TH.Q (TH.TExp a)|, where |TH| denotes the library for Template Haskell,
|TH.Q| is the quotation monad of Template Haskell (used to look up
identifiers and generate fresh names), and |TH.TExp a| is the parse
tree for an expression that returns a value of type |a|. In the
latter, |a| is a phantom variable; type |TH.TExp a| is just a wrapper for
|TH.Exp|, the (untyped) parse tree of an expression in Template
Haskell. Hence, the translator from |Qt a| to |Dp a| is forced to
re-infer all the type information for the subterms of the term of type
|Qt a|.  This is also why we translate the |Maybe| monad as a special
case, rather than supporting overloading for monad operations.

%%  As we noted in the introduction, rather than build a special-purpose tool for
%%  each QDSL, it should be possible to design a single tool for each host language.
%%  In the conclusion, we sketch the design of a general purpose tool for Haskell QDSL,
%%  including support for type inference and overloading.

\input{table}

We measured the behaviour of five benchmark programs.
\begin{center}
\begin{tabular}{l@@{~}||@@{~}l}
IPGray     & Image Processing (Grayscale)  \\
IPBW       & Image Processing (Black and White) \\
FFT        & Fast Fourier Transform \\
CRC        & Cyclic Redundancy Check \\
Windowing  & Average array in a sliding window \\
\end{tabular}
\end{center}
Table~\ref{thetable} lists the results. Columns \hct\ and \hrt\
list compile-time and run-time in Haskell, and \cct\ and \crt\
list compile-time and run-time in C.
Runs for CDSL are shown both with and without common subexpression elimination (CSE),
which is supported by a simple form of observable sharing. QDSL does not require CSE,
since the normalisation algorithm preserves sharing. One benchmark, FFT, exhausts
memory without CSE. All benchmarks produce essentially the same C for both QDSL
and CDSL, which run in essentially the same time. The one exception is FFT, where
class |Syn| appears to introduce spurious conversions that increase the
runtime.

Measurements were done on a PC with a quad-core Intel i7-2640M CPU
running at 2.80 GHz and 3.7 GiB of RAM, with GHC Version 7.8.3 and
GCC version 4.8.2, running on Ubuntu 14.04 (64-bit).


%\section{The design of Haskell DSL}
%\label{sec:tool}


\section{Related work}
\label{sec:related}

Domain specific languages are becoming increasingly popular as a way
to deal with software complexity. Yet, they have a long and rich
history \citep{Bentley:1986:PPL:6424.315691}.

In this paper we have, like many other DSL writers, used Haskell as it
has proven to be very suitable for \emph{embedding} domain specific
languages \citep{Gill:14:DSLs-and-Synthesis}.  Examples include
\citet{reid1999prototyping, hudak1997domain, bjesse1998lava}.

In this paper we have specifically built on the technique of combining
deep and shallow embeddings \citep{SvenningssonA12} and
contrasted it with our new QDSL technique. Languages which have used
this technique include Feldspar \citep{FELDSPAR}, Obsidian
\citep{svensson2011obsidian}, Nikola \citep{NIKOLA}, Hydra
\citep{giorgidze2011embedding} and Meta-Repa \citep{ankner2013edsl}.

The vector type used in this paper is one of several types which
enjoy fusion in the CDSL framework. Other examples include push
arrays \citep{claessen2012expressive} and sequential arrays
and streams as used in Feldspar \citep{feldspar-github}.

The loss of sharing when implementing embedded DSLs was identified by
\citet{o1993generating} in the context of embedded circuit descriptions.
\citet{claessen1999observable} proposed to introduce a little
bit of impurity in Haskell, referred to as \emph{observable sharing}
to be able to recover from the loss of sharing. Later, \citet{gill2009type}
proposed a somewhat safer way of recover sharing, though still ultimately
relying on impurity.

The underlying idea for QDSLs was established by \citet{CheneyLW13}.

\section{Conclusion}
\label{sec:conclusion}

We have compared CDSLs and QDSLs, arguing that QDSLs offer competing
expressiveness and efficiency. CDSLs often (but not always) mimic the
syntax of the host language, and often (but not always) perform
normalisation in the host languages, while QDSLs (always) steal the
syntax of the host language, and (always) ensure the subformula property,
at the cost of requiring a normaliser, one per host language.

The subformula property may have applications in DSLs other that
QDSLs. For instance, after Section~\ref{sec:option} of this paper was
drafted, it occurred to us that a different approach to options in
CDSL would be to extend type |Dp| with constructs for type |Maybe|.
So long as type |Maybe| does not appear in the input or output of the
program, a normaliser that ensures the subformula property could
guarantee that C code for such constructs need never be generated.

As we noted in the introduction, rather than build a special-purpose tool for
each QDSL, it should be possible to design a single tool for each host language.
Our next step is to design Haskell QDSL, with the following features.
\begin{itemize}
\item Full-strength type inference for the terms returned from typed
  quasi-quotations, restoring type information currently discarded by GHC.
\item Based on the above, full support for type classes and overloading
  within quasi-quotation.
\item The user may choose either an ADT or GADT representation of the
  term returned by typed quasi-quotation, whichever is more convenient.
\item A normaliser to ensure the subformula property, which works with any
  datatype declared in Haskell.
\item The user may supply a type environment indicating which constants
  (or free variables) may appear in typed quasi-quotations.
\end{itemize}
Such a tool could easily subsume the special-purpose translator from
|Qt| to |Dp| described at the beginning of Section~\ref{sec:evaluation},
and lift most of its restrictions. For instance,
the current prototype is restricted to the |Maybe| monad, while the
envisioned tool will work with any monad.

Moli\`{e}re's Monsieur Jourdain was bemused to discover he had been
speaking prose his whole life. Similarly, many of us have used QDSLs for
years, if not by that name. DSL via quotation is the heart of Lisp
macros, Microsoft LINQ, and Scala LMS, to name but three. We hope that
by naming the concept and drawing attention to the central benefits of
normalisation and the subformula propety, we may help the concept to
flower further for decades to come.

\paragraph*{Acknowledgement} This work was funded by EPSRC Grant
EP/K034413/1. Shayan Najd is a recipient of the Google Europe
Fellowship in Programming Technology, and this research is supported
in part by this Google Fellowship. %% <-- Google's requested format
Josef Svenningsson is a SICSA Visiting Fellow and is funded by a
HiPEAC collaboration grant and by the Swedish Foundation for Strategic
Research, under grant RawFP.

\bibliographystyle{plainnat}
\bibliography{paper}

\end{document}

