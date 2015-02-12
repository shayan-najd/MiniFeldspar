\documentclass[authoryear]{sigplanconf}

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
%format ... = "\cdots"
%format A_1
%format A_k
%format Arr_Fu

% US Letter page size
%\pdfpagewidth=8.5in
%\pdfpageheight=11in

% The following \documentclass options may be useful:
%
% 10pt          To set in 10-point type instead of 9-point.
% 11pt          To set in 11-point type instead of 9-point.
% authoryear    To obtain author/year citation style instead of numeric.

%\usepackage[round]{natbib}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{stmaryrd}
\usepackage{proof}
\usepackage{xspace}
\usepackage[pdfauthor={Shayan Najd,Sam Lindley,Josef Svenningsson,Philip Wadler}
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

\newtheorem{theorem}{Theorem}[section]
\newtheorem{proposition}[theorem]{Proposition}

\makeatletter
\renewcommand\bibsection%
{
  \section{\refname
    \@@mkboth{\MakeUppercase{\refname}}{\MakeUppercase{\refname}}}
}
\makeatother

\newcommand{\flushr}{\protect\\{}\mbox{~}\hfill}%{\flushright\vspace{-2ex}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\conferenceinfo{ICFP 2015}{August 31--September 2, 2015, Vancouver, Canada.}
\copyrightyear{2015}
\copyrightdata{...}
\doi{...}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\title{Everything old is new again:\\
       Quoted Domain Specific Languages}

\authorinfo{Shayan Najd}
           {The University of Edinburgh}
           {sh.najd@@ed.ac.uk}
\authorinfo{Sam Lindley}
           {The University of Edinburgh}
           {sam.lindley@@ed.ac.uk}
\authorinfo{Josef Svenningsson}
           {Chalmers University of Technology}
           {josefs@@chalmers.se}
\authorinfo{Philip Wadler}
           {The University of Edinburgh}
           {wadler@@inf.ed.ac.uk}

% \numberofauthors{4}
% \author{
% \alignauthor
% Shayan Najd\\
%        \affaddr{The University of Edinburgh}\\
%        \email{sh.najd@@ed.ac.uk}
% \alignauthor
% Sam Lindley\\
%         \affaddr{The University of Edinburgh}\\
%         \email{sam.lindley@@ed.ac.uk}
% \alignauthor
% Josef Svenningsson\\
%        \affaddr{Chalmers University of Technology}\\
%        \email{josefs@@chalmers.se}
% \and
% \alignauthor
% Philip Wadler\\
%        \affaddr{The University of Edinburgh}\\
%        \email{wadler@@inf.ed.ac.uk}
% }

\maketitle

\begin{abstract}

Fashions come, go, return. We describes a new approach to domain
specific languages (DSLs), called Quoted DSLs (QDSLs), that resurrects two old ideas: quoted
terms for domain specific languages, from McCarthy's Lisp of 1960, and
the subformula property, from Gentzen's natural deduction of 1935.
Quoted terms allow the domain specific language to share the syntax
and type system of the host language. Normalising quoted terms ensures
the subformula property, which provides strong guarantees, e.g., that
one can use higher-order or nested code in the source while
guaranteeing first-order or flat code in the target, or using types
to guide loop fusion.  We give three examples of QDSL: QFeldspar (a
variant of Feldsar), P-LINQ for F\#, and some uses of Scala LMS.
We also provide a comparison between QDSLs and
traditional Embedded DSL (EDSLs).

\end{abstract}

\category{D.1.1}{Applicative (Functional) Programming}{}
\category{D.3.1}{Formal Definitions and Theory}{}
\category{D.3.2}{Language Classifications}{Applicative (functional) languages}

% \terms{Theory}

\keywords
domain-specific language, DSL, EDSL, QDSL,
embedded language,
quotation, normalisation, subformula property

\section{Introduction}
\label{sec:introduction}

% The difficulty lies not so much in developing new ideas as in escaping from old ones.
% - John Maynard Keynes

% I can't understand why people are frightened of new ideas. I'm frightened of the old ones.
% - John Cage

\begin{quote}
Don't throw the past away \\
You might need it some rainy day \\
Dreams can come true again \\
When everything old is new again
\flushr -- Peter Allen and Carole Sager
\end{quote}
\vspace{2ex}

Implementing domain-specific languages (DSLs) via quotation is one of
the oldest ideas in computing, going back at least to macros in Lisp.
Today, a more fashionable technique is Embdedded DSLs (EDSLs), which
may use shallow embedding, deep embedding, or a combination of the
two. Our goal in this paper is to reinvigorate the idea of building
DSLs via quotation, by introducing a new approach that depends
crucially on normalising the quoted term, which we dub Quoted DSLs
(QDSLs).

\vspace{2ex}
\begin{quote}
Imitation is the sincerest of flattery.
\flushr --- Charles Caleb Colton
\end{quote}
\vspace{2ex}

\citet{cheney:linq} describes a DSL for language-integrated query in
F\# that translates into SQL. The approach relies on the key features
of QDSL---quotation, normalisation of quoted terms, and the subformula
property---and the paper conjectures that these may be useful in other
settings.  We are particularly interested in DSLs that perform staged
computation, where at generation-time we use host code to
compute target code that is to be executed at run-time.

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

\begin{quote}
Perhaps we may express the essential properties of such a normal proof
by saying: it is not roundabout.
\flushr --- Gerhard Gentzen
\end{quote}
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
work, such as \citet{cheney:linq} uses a call-by-name normalisation
algorithm that performs full beta-reduction, which may cause
computations to be repeated.  Here we present call-by-value and
call-by-need normalisation algorithms, which guarantee to preserve
sharing of computations.

\begin{quote}
Good artists copy, great artists steal.
\flushr --- Picasso
\end{quote}
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

\begin{quote}
Try to give all of the information to help others to judge the value
of your contribution; not just the information that leads to judgment
in one particular direction or another.
\flushr --- Richard Feynman
\end{quote}
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
the target language. Neither the P-LINK of \citet{cheney:linq}
nor the QFeldspar of this paper satisfy this property.
It is possible to ensure the property with additional preprocessing;
we clarify the tradeoff between ease of
implementation and ensuring safe compilation to target at
compile-time rather than run-time.

\begin{quote}
TODO: some quotation suitable for contributions (or summary)
\end{quote}
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
\citep{cheney:linq} and the Scala LMS implementations of query
and [TODO: what else?], and argue that these are instances of QDSL.
(Section~\ref{sec:other-qdsls}.)

\item To argue that, from the user's point of view, the QDSL
implementation of Feldspar is conceptually easier to understand than
the EDSL implementation of Feldspar, by a detailed comparison of
the user interface of the two implementations (Section~\ref{sec:qdsl-vs-edsl}.)

\end{itemize}
Section~\ref{sec:related} describes related work, and
Section~\ref{sec:conclusion} concludes.

% \section{A QDSL variant of Feldspar}
% \label{sec:qfeldspar}

% \section{The subformula property}
% \label{sec:subformula}

% \section{Other examples of QDSLs}
% \label{sec:other-qdsls}

% \subsection{F\# P-LINQ}
% \label{subsec:linq}

% \subsection{Scala LMS}
% \label{subsec:lms}

% \section{A comparison of QDSL and EDSL}
% \label{sec:qdsl-vs-edsl}

% \section{Related work}
% \label{sec:related}

% \section{Conclusion}
% \label{sec:conclusion}

% \bibliographystyle{plainnat}
% \bibliography{paper}

% \end{document}


\section{A QDSL variant of Feldspar}
\label{sec:qfeldspar}

Feldspar is an EDSL for writing signal-processing software, that
generates code in C \citep{FELDSPAR}. We present a variant,
QFeldspar, that follows the structure of the previous design closely,
but using the methods of QDSL rather than EDSL. We make a detailed
comparison of the QDSL and EDSL designs in Section~\ref{sec:qdsl-vs-edsl}.

% \subsection{Design}
% \label{subsec:qfeldspar-design}

\subsection{The top level}
\label{subsec:top}

In QFeldspar, our goal is to translate a quoted term to C code, so we
also assume a type |C| that represents code in C. The top-level
function of QFeldspar has the type:
\begin{spec}
qdsl :: (Rep a , Rep b) => Qt (a -> b) -> C
\end{spec}
which generates a \texttt{main} function that takes an argument
of type |a| and returns a result of type |b|.

Not all types representable in Haskell are easily representable
in the target language, C. For instance, we do not wish our target
C code to manipulate higher-order functions.  The argument
type |a| and result type |b| of the main function must be representable,
which is indicated by the type-class restrictions |Rep a| and |Rep b|.
Representable types include integers, floats, and pairs where the components
are both representable.
\begin{spec}
instance Rep Int
instance Rep Float
instance (Rep a, Rep b) => Rep (a,b)
\end{spec}
It is easy to add triples and larger tuples.

\subsection{An introductory example}
\label{subsec:power}

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
    [|| \x -> if x == 0  then 0
                         else 1 / ($$(power (-n)) x) ||]
  else if n == 0 then
    [|| \x -> 1 ||]
  else if even n then
    [|| \x -> $$sqr ($$(power (n `div` 2)) x) ||]
  else
    [|| \x -> x * ($$(power (n-1)) x) ||]

sqr  ::  Qt (Float -> Float)
sqr  =   [|| \y -> y * y ||]
\end{code}
The typed quasi-quoting mechanism of Template Haskell is used to
indicate which code executes at which time.  Unquoted code executes at
generation-time while quoted code executes at run-time. Quoting is
indicated by |[||...||]| and unquoting by |$$(...)|.

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
With the exception of the top-level term, all of the overhead of
lambda abstraction and function application has been removed; we
explain below why this is guaranteed by Gentzen's subformula property.
From the normalised term it is easy to generate the desired C code:
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
By default, we always generate a routine called \texttt{main}; it
is easy to provide the name as an additional parameter if required.

Depending on your point of view, quotation in this form of QDSL is
either desirable, because it makes manifest the staging, or
undesirable because it is too noisy. We return to this point in
Section~\ref{sec:TODO}.  QDSL enables us to ``steal'' the entire
syntax of the host language for our DSL.  The EDSL approach can use
the same syntax for arithmetic operators, but must use a different
syntax for equality tests and conditionals, as we will see in
Section~\ref{sec:qdsl-vs-edsl}.

Within the quotation brackets there appear lambda abstractions and
function applications, while our intention is to generate first-order
code. How can the QFeldspar user be certain that such function
applications do not render transformation to first-order code
impossible or introduce additional runtime overhead?  The answer is
Gentzen's subformula property.


\subsection{The subformula property}
\label{subsec:subformula}

Gentzen's subformula property guarantees that any proof can be
normalised so that the only formulas that appear within it are
subformulas of one of the hypotheses or of the conclusion of the
proof.  Viewed through the lens of Propositions as Types
\citep{wadler-2015}, also known as the Curry-Howard Isomorphism,
Gentzen's subformula property guarantees that any term can be
normalised so that the type of each of its subterms is a subtype of
either the type of one of its free variables (corresponding to
hypotheses) or the term itself (corresponding to the conclusion).
Here the subtypes of a type are the type itself and the subtypes of
its parts, where the parts of |a -> b| are |a| and |b|, the parts of
|(a,b)| are |a| and |b|, and the only part of |Arr a| is |a|, and that
types |int| and |float| have no parts.

Further, it is easy to sharpen Gentzen's proof to guarantee a sharpened
subformula property: any term can be normalised so that the type of
each of its proper subterms is a proper subtype of either the type of
one of its free variables (corresponding to hypotheses) or the term
itself (corresponding to the conclusion).  Here the proper subterms of
a term are all subterms save for free variables and the term itself,
and the proper subtypes of a type are all subtypes save for the type
itself.

In the example of the previous subsection, the sharpened subformula
property guarantees that after normalisation a term of type |float ->
float| will only have proper subterms of type |float|, which is indeed
true for the normalised term.

(Careful readers will have noticed a small difficulty.  One of the
free variables of our quoted term is multiplication over floats.  In
Haskell, |m*n| abbreviates |(((*) m) n)|, which has |((*) m)| as a
subterm, and the type of |(*)| is |(float -> (float -> float))|, which has
|(float -> float)| as a subtype. We alleviate the difficulty by a
standard trick: each free variable is assigned an arity and must
always be fully applied. Taking |(*)| to have arity 2 requires we
always write |m*n| in our code. Then we may, as natural, regard |m|
and |n| as the only subterms of |m*n|, and |float| as the only subtype
of the type of |(*)|. Details appear in Section~\ref{sec:subformula}.)

\subsection{Maybe}
\label{subsec:maybe}

In the previous code, we arbitrarily chose that raising zero to a
negative power yields zero. Say that we wish to exploit the |Maybe| type
to refactor the code, separating identifying the exceptional case
(negative exponent of zero) from choosing a value for this case (zero).
We decompose |power| into two functions |power'| and |power''|, where the first
returns |Nothing| in the exceptional case, and the second maps |Nothing|
to a suitable default value.

The |Maybe| type is a part of the standard prelude.
\begin{spec}
data Maybe a  =   Nothing | Just a

return        ::  a -> Maybe a
return        =   Just

(>>=)         ::  Maybe a -> (a -> Maybe b) -> Maybe b
m >>= k       =   case m of
                    Nothing  -> Nothing
                    Just x   -> k x

maybe         ::  b -> (a -> b) -> Maybe a -> b
maybe x g m   =   case m of
                    Nothing  -> x
                    Just y   -> g y
\end{spec}

Here is the refactored code.
\begin{code}
power' :: Int -> Qt (Float -> Maybe Float)
power' n =
  if n < 0 then
    [|| \x ->  if x == 0  then Nothing
                          else do  y <- $$(power' (-n)) x
                                   return (1 / y) ||]
  else if n == 0 then
    [|| \x -> return 1 ||]
  else if even n then
    [|| \x -> do  y <- $$(power' (n `div` 2)) x
                  return ($$sqr y) ||]
  else
    [|| \x -> do  y <- $$(power' (n-1)) x
                  return (x * y) ||]

power''      ::  Int -> Qt (Float -> Float)
power'' n = [|| \ x ->  maybe 0 (\y -> y) ($$(power' n) x)||]
\end{code}
Here |sqr| is as before. Evaluation and normalisation of
|power (-6)| and |power'' (-6)| yield identical terms
(up to renaming), and hence applying |qdsl| to these yields
identical C code.

The subformula property is key: because the final type of the result
does not involve |Maybe| it is certain that normalisation will remove
all its occurrences.  
Occurrences of |do| notation are
expanded to applications of |(>>=)|, as usual.
Rather thank taking |return|, |(>>=)|, and |maybe| as free variables
(whose types have subtypes involving |Maybe|),
we treat them as known definitions to be eliminated by the
normaliser.  
The |Maybe| type is essentially a sum type, and normalisation for
these is as described in Section~\ref{sec:subformula}.

We have chosen not to make |Maybe| a representable type, which
prohibits its use as argument or result of the top-level function
passed to |qdsl|. An alternative choice is possible, as we will see
when we consider arrays, in Section~\ref{subsec:arrays} below.


\subsection{While}
\label{subsec:while}

Code that is intended to compile to a @while@ loop in C is indicated
in QFeldspar by application of |while|.
\begin{spec}
while :: (Rep s) => Qt ((s -> Bool) -> (s -> s) -> s -> s)
\end{spec}
Rather than using side-effects, |while| takes three
arguments: a predicate over the current state, of type |s -> Bool|; a
function from current state to new state, of type |s -> s|; and an
initial state of type |s|; and it returns a final state of type |s|.
Since we intend to compile the while loop to C, the type
of the state is constrained to representable types.

We can define a |for| loop in terms of a |while| loop.
\begin{code}
for :: (Rep s) => Qt (Int -> s -> (Int -> s -> s) -> s)
for =  [|| \n s_0 b -> snd (while  (\(i,s) -> i < n)
                                   (\(i,s) -> (i+1 , b i s))
                                   (0, s_0)) ||]
\end{code}
The state of the |while| loop is a pair consisting of a counter and
the state of the |for| loop. The body |b| of the |for| loop is a function
that expects both the counter and the state of the |for| loop.
The counter is discarded when the loop is complete, and the final state
of the |for| loop returned.

As an example, we can define Fibonacci using a |for| loop.
\begin{code}
fib :: Qt (Int -> Int)
fib =  [|| \n -> $$for n (\(a,b) -> (b,a+b)) (0,1) |]]
\end{code}

Again, the subformula property plays a key role.
As explained in Section~\ref{subsec:subformula}, primitives of the
language to be compiled, such as |(*)| and |while|, are treated as
free variables of a given arity.
As will be explained in Section~\ref{subsec:rank},
we can ensure that after normalisation every occurence of |while|
has the form
\begin{spec}
while (\s -> ...) (\s -> ...) (...)
\end{spec}
where the first ellipses has type |Bool|,
and both occurrences of |s| and the second and third ellipses
all have the same type. 

Unsurprisingly, and in accord with the subformula property, for each
occurrence of |while| in the normalised code will contain subterms
with the type of its state. The restriction of state to representable
types increases the utility of the subformula property. For instance,
since we have chosen that |Maybe| is not a representable type, we can
ensure that any top-level function without |Maybe| in its type will
normalise to code not containing |Maybe| in the type of any subterm.
An alternative choice is possible, as we will see in the next section.

Complete normalisation of terms is sometimes impossible or
undesirable.  The extent of normalisation is controlled by
introducing uninterpreted constants, such as |while|.
In a context with recursion, 
we take |fix :: (a -> a) -> a| as an uninterpreted constant.
In a context where we wish to avoid unfolding a reduction |L M|,
we take |id :: a -> a| as an uninterpreted constant,
and replace |L M| by |id L M|.



\subsection{Arrays}
\label{subsec:arrays}

A key feature of Feldspar is its distinction between two types of
arrays, manifest arrays |Arr| which may appear at run-time, and
``pull arrays'' |Vec| which are eliminated by fusion at generation-time.
Again, we exploit the subformula property to ensure
no subterms of type |Vec| remain in the final program.

The type |Arr| of manifest arrays is simply Haskell's array type,
specialised to arrays with integer indices and zero-based indexing.
The type |Vec| of pull arrays is defined in terms of existing types,
as a pair consisting of the length of the array and a function
that given an index returns the array element at that index.
\begin{spec}
type Arr a  =  Array Int a
data Vec a  =  Vec Int (Int -> a)
\end{spec}
Values of type |Arr| are representable, assuming that the
element type is representable, while values of type |Vec|
are not representable.
\begin{spec}
instance (Rep a) => Rep (Arr a)
\end{spec}

For arrays, we assume the following primitive operations.
\begin{spec}
makeArr  ::  (Rep a) => Int -> (Int -> a) -> Arr a
lenArr   ::  (Rep a) => Arr a -> Int
ixArr    ::  (Rep a) => Arr a -> Int -> a
\end{spec}
The first populates a manifest array of the given
size using the given indexing function, the second
returns the length of the array, and the third returns
the array element at the given index.

We define functions to convert between the two representations in the
obvious way.
\begin{code}
toArr        ::  Qt (Vec a -> Arr a)
toArr        =   [|| \(Vec n g) -> makeArr n (\ x -> g x) ||]

toVec        ::  Qt (Arr a -> Vec a)
toVec        =   [|| \a -> Vec (lenArr a) (\i -> ixArr a i) ||]
\end{code}

It is straightforward to define operations on vectors,
including combining corresponding elements of two vectors,
summing the elements of a vector, dot product of two vectors,
and norm of a vector.
\begin{code}
zipVec   ::  Qt ((a -> b -> c) -> Vec a -> Vec b -> Vec c)
zipVec   =   [||  \f (Vec m g) (Vec n h) ->
                        Vec (m `min` n) (\i -> f (g i) (h i)) ||]

sumVec   ::  (Rep a, Num a) => Qt (Vec a -> a)
sumVec   =   [|| \(Vec n g) -> $$for n 0 (\i x -> x + g i) ||]

dotVec   ::  (Rep a, Num a) => Qt (Vec a -> Vec a -> a)
dotVec   =   [|| \u v -> $$sumVec ($$zipVec (*) u v) ||]

normVec  ::  Qt (Vec Float -> Float)
normVec  =   [|| \v -> sqrt ($$scalarProd v v ||]
\end{code}
The second of these uses the |for| loop defined in
Section~\ref{subsec:while}, the third is defined using
the first two, and the fourth is defined using the third.
Recall that our sharpened subformula property required
that |(*)| be fully applied, so before normalisation
|(*)| is expanded to |\x y -> x*y|.

Our final function cannot accept |Vec| as input, since
the |Vec| type is not representable, but it can accept
|Arr| as input. Invoking |qdsl (normVec . toVec)|
produces the following C code.
\begin{lstlisting}
float main(float[] a) {
  float x = 0;
  int i = 0;
  while (i < lenArr a) {
    x = x + a[i] * a[i];
    i = i+1;
  }
  return sqrt(x);
}  
\end{lstlisting}
[TODO: Shayan to check that above is correct.]

Types and the subformula property help us to guarantee fusion.
The subformula property guarantees that all occurrences
of |Vec| must be eliminated, while occurrences of |Arr| will remain.
There are some situations where fusion is not beneficial, notably
when an intermediate vector is accessed many times fusion will cause
the elements to be recomputed.  An alternative is to materialise the
vector in memory with the following function.
\begin{code}
memorise  ::  Syn a => Qt (Vec a -> Vec a)
memorise  =   [|| toVec . toArr ||]
\end{code}
For example, if
\begin{spec}
blur :: Syn a => Vec a -> Vec a
\end{spec}
averages adjacent elements of a vector, then one may choose to
compute either
\begin{center}
|[||$$blur . $$blur||]| ~~~or~~~ |[||$$blur . $$memorise . $$blur||]|
\end{center}
with different trade-offs between recomputation and memory usage.
Strong guarantees for fusion in combination with |memorize| gives
the programmer a simple interface which provides powerful optimisation
combined with fine control over memory usage.

[TODO: The subformula property guarantees that all occurrences
of |Vec| will vanish from the final program. The same guarantee
applies regardless of whether |Vec| is defined as ``pull arrays''
or ``push arrays''. Does this mean the difference is irrelevant
for QFeldspar?]

\subsection{Implementation}
\label{subsec:implementation}

\input{table}

The original Feldspar generates values of an algebraic type
(called |Exp a| in \citet{svenningsson:combining}), with constructs
that represent |while| and manifest arrays similar to those
above. A backend then compiles values of type |Exp a| to C code.
QFeldspar provides a transformer from |Qt a| to |Exp a|, and
shares the Feldspar backend.

The transformer from |Qt| to |Exp| performs the following steps.
\begin{itemize}
\item It expands identifiers connected with the types |(,)|, |Maybe|
  and |Vec|.
    \begin{itemize}
    \item For |(,)|, identifiers |fst| and |snd|.
    \item For |Maybe|, identifiers |return|, |(>>=)|, and |maybe|.
    \item For |Vec|, there are no relevant identifiers.
    \end{itemize}
\item It normalises the term to ensure the subformula property,
  using the rules of Section~\ref{sec:subformula}.
  The normaliser does not support all Haskell data types,
  but does include special-purpose rules for |Maybe| and |Vec|.
\item It traverses the term, converting |Qt| to |Exp|.
  It checks that only permitted primitives appear in |Qt|, and translates
  these to their corresponding representation in |Exp|. Permitted primitives include:
  |(==)|, |(<)|, |(+)|, |(*)|, and similar, plus
  |while|, |makeArr|, |lenArr|, and |ixArr|.
\end{itemize}

An unfortunate feature of typed quasiquotation in GHC is that the
implementation discards all type information when creating the
representation of a term.  Type |Qt a| is equivalent to |TH.Q (TH.TExp
a)|, where |TH| denotes the library for Template Haskell, |TH.Q| is
the quotation monad of Template Haskell (used to look up identifiers
and generate fresh names), and |TH.TExp a| is the parse tree for a
quoted expression returning a value of type |a|. Type |TH.TExp a| is
just a wrapper for |TH.Exp|, the (untyped) parse tree of an expression
in Template Haskell, where |a| is a phantom type variable. Hence, the
translator from |Qt a| to |Exp a| is forced to re-infer all the type
information for the subterms of the term of type |Qt a|.  This is why
we translate the |Maybe| monad as a special case, rather than
supporting overloading for monad operations.  [TODO: say something
about how overloading for arithmetic is handled.]

%%  As we noted in the introduction, rather than build a special-purpose tool for
%%  each QDSL, it should be possible to design a single tool for each host language.
%%  In the conclusion, we sketch the design of a general purpose tool for Haskell QDSL,
%%  including support for type inference and overloading.

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
Figure~\ref{fig:thetable} lists the results. Columns \hct\ and \hrt\
list compile-time and run-time in Haskell, and \cct\ and \crt\
list compile-time and run-time in C.
Runs for CDSL are shown both with and without common subexpression elimination (CSE),
which is supported by a simple form of observable sharing. QDSL does not require CSE,
since the normalisation algorithm preserves sharing. One benchmark, FFT, exhausts
memory without CSE. All benchmarks produce essentially the same C for both QDSL
and CDSL, which run in essentially the same time. The one exception is FFT, where
Feldspar appears to introduce spurious conversions that increase the
runtime.

Measurements were done on a PC with a quad-core Intel i7-2640M CPU
running at 2.80 GHz and 3.7 GiB of RAM, with GHC Version 7.8.3 and
GCC version 4.8.2, running on Ubuntu 14.04 (64-bit).

\section{The subformula property}
\label{sec:subformula}

%% Types
\newcommand{\zero}{\mathbf{0}}
\newcommand{\one}{\mathbf{1}}
\newcommand{\arrow}[2]{#1\rightarrow#2}
\newcommand{\product}[2]{#1\times#2}
\newcommand{\coproduct}[2]{#1+#2}
\newcommand{\rec}[2]{\mu#1.#2}

%% Expressions
\newcommand{\key}{\mathbf}

\newcommand{\app}{\;}

\newcommand{\expvar}[1]{#1}
\newcommand{\expconst}[2]{#1~#2}
\newcommand{\expunit}{()}
\newcommand{\expabs}[3]{\lambda#1.#3}
\newcommand{\expapp}[2]{#1 \app #2}
\newcommand{\explet}[3]{\key{let}\ #1=#2\ \key{in}\ #3}
\newcommand{\exppair}[2]{(#1, #2)}
\newcommand{\expfst}[1]{\key{fst}~#1}
\newcommand{\expsnd}[1]{\key{snd}~#1}
\newcommand{\expinl}[2]{\key{inl}~#1}
\newcommand{\expinr}[2]{\key{inr}~#2}
\newcommand{\expcase}[5]
  {\key{case}\ #1\ \key{of}\ \{\key{inl}\ #2.\,#3;\; \key{inr}\ #4.\,#5\}}

%% Meta language stuff
\newcommand{\subst}[3]{#1[#2:=#3]}
\newcommand{\fv}[1]{\mathit{FV}(#1)}
\newcommand{\rewrite}[1]{\mathbin{\mapsto_{#1}}}
\newcommand{\hole}{[~]}
\newcommand{\nv}{P}

%% Typing
\newcommand{\intro}{\mathcal{I}}
\newcommand{\elim}{\mathcal{E}}
\newcommand{\inference}[3]{\infer[\mathsf{#2}]{#3}{#1}}

\newcommand{\figterm}{
\begin{figure*}[t]
\[
\begin{array}{l@@{\quad}rcl}
\text{Types} & A,B,C & ::=&
% \one
  \iota           \mid
  \arrow{A}{B}    \mid
  \product{A}{B}  \mid
  \coproduct{A}{B}
\\[1ex]
\text{Terms} & L,M,N & ::= &
% \expunit                	\mid
  \expvar{x}              	\mid
  \expconst{c}{\overline{M}}	\mid
  \expabs{x}{A}{N}              \mid
  \expapp{L}{M}                 \mid
  \explet{x}{M}{N}              \mid
  \exppair{M}{N}                \mid
  \expfst{L}                   	\mid
  \expsnd{L}                    \mid \\&&&
  \expinl{M}{\tm{B}}		\mid
  \expinr{\tm{A}}{N}           	\mid
  \expcase{L}{x}{M}{y}{N}
\\[1ex]
\text{Values} & V,W & ::= &
% \expunit         \mid
  \expvar{x}       \mid
  \expabs{x}{A}{N} \mid
  \exppair{V}{W}   \mid
  \expinl{V}{B}    \mid
  \expinr{A}{W}
\end{array}
\]
\caption{Types, Terms, and Values}
\label{fig:term}
\end{figure*}
}

\newcommand{\figtyping}{
\begin{figure*}[h]
\[
\begin{array}{@@{}ll@@{}}
\fbox{$\Gamma \vdash M:A$}
\\~\\
\inference
{x:A \in \Gamma}
{\mathbf{Ax}}
{
  \Gamma \vdash x:A
}
&
\inference
{}
{\one}
{
   \Gamma \vdash \expunit:\one
}
\\~\\
\inference
{
  \Gamma, x:A \vdash N:B
}
{{\to}\intro}
{
  \Gamma \vdash \expabs{x}{A}{N}:\arrow{A}{B}
}
&
\inference
{
  \Gamma \vdash L:\arrow{A}{B}
& \Gamma \vdash M:A
}
{{\to}\elim}
{
  \Gamma \vdash \expapp{L}{M}:B
}
\\~\\
\inference
{
  \Gamma \vdash M:A
  &
  \Gamma, x:A \vdash N:B
}
{\mathbf{let}}
{
  \Gamma \vdash \explet{x}{M}{N}:B
}
&
\inference
{
  \Gamma \vdash M:A
  &
  \Gamma \vdash N:B
}
{{\times}\intro}
{
  \Gamma \vdash \exppair{M}{N}:\product{A}{B}
}
\\~\\
\inference
{
  \Gamma \vdash L:\product{A}{B}
}
{{\times}\elim_1}
{
  \Gamma \vdash \expfst{L}:A
}
&
\inference
{
  \Gamma \vdash L:\product{A}{B}
}
{{\times}\elim_2}
{
  \Gamma \vdash \expsnd{L}:B
}
\\~\\
\inference
{
  \Gamma \vdash M:A
}
{{+}\intro_1}
{
  \Gamma \vdash \expinl{M}{B}:\coproduct{A}{B}
}
&
\inference
{
  \Gamma \vdash N:B
}
{{+}\intro_2}
{
  \Gamma \vdash \expinr{A}{N}:\coproduct{A}{B}
}
\\~\\
\inference
{
  \Gamma \vdash L:\coproduct{A}{B}
&
  \Gamma, x:A \vdash M:C
&
  \Gamma, y:B \vdash N:C
}
{{+}\elim}
{
  \Gamma \vdash \expcase{L}{x}{M}{y}{N}:C
}
\end{array}
\]
\caption{Typing Rules}
\label{fig:typing}
\end{figure*}
}

\newcommand{\fignf}{
\begin{figure*}[t]
\[
\begin{array}{l@@{\quad}rcl}
\text{Neutral Forms}
  & Q & ::= & \expapp{x}{W}
         \mid \expconst{c}{\overline{W}}
         \mid \expapp{Q}{W}
         \mid \expfst{x}
         \mid \expsnd{x}
\\[1ex]
\text{Normal Values}
  & V,W & ::= & \expvar{x}
           \mid \expabs{x}{A}{N}
           \mid \exppair{V}{W}
           \mid \expinl{V}{B}
           \mid \expinr{A}{W}
\\[1ex]
\text{Normal Forms}
  & N,M & ::= & Q
          \mid  V
          \mid \expcase{z}{x}{N}{y}{M}
          \mid \explet{x}{Q}{N}
\\
\end{array}
\]
\caption{Normal Forms}
\label{fig:nf}
\end{figure*}
}

\newcommand{\fignorm}{
\begin{figure*}[t]
Phase 1 (let-insertion)
\[
  \begin{array}{lcl}
   F &\mathbin{::=}&
       \expconst{c}{(\overline{V},\hole,\overline{M})} \mid 
       \expapp{M}{\hole}                               \mid 
       \exppair{\hole}{N}                              \mid 
       \exppair{M}{\hole}                              \mid 
       \expfst{\hole}                                  \mid 
       \expsnd{\hole}                                  \mid % \\ &&
       \expinl{\hole}{B}                               \mid 
       \expinr{A}{\hole}                               \mid 
       \expcase{\hole}{x}{M}{y}{N} \\
 \end{array}
\]%
\[
(\mathit{let})
~~F[\nv]
~\rewrite{1}~
\explet{x}{\nv}{F[x]}, \quad x \text{ fresh}
\]

\vspace{2ex}

Phase 2 (symbolic evaluation)
\[
G \mathbin{::=}
    \expapp{\hole}{V} \mid \explet{x}{\hole}{N}
\]
\[
\begin{array}{lllll}
(\kappa.{\mathit{let}})
& G[\explet{x}{\nv}{N}]
& \rewrite{2}
& \explet{x}{\nv}{G[N]},
& x \notin \fv{G} \\

(\kappa.{\mathit{case}})
& G[\expcase{z}{x}{M}{y}{N}]
& \rewrite{2}
& \expcase{z}{x}{G[M]}{y}{G[N]},
& x,y \notin \fv{G} \\

% (\kappa.{\mathit{case}})
% & G[\expcase{z}{x}{M}{y}{N}] & \rewrite{2}\\
% \multicolumn{3}{@@{}l@@{}}
% {\qquad\qquad\qquad\quad \expcase{z}{x}{G[M]}{y}{G[N]},} & \quad x,y \notin \fv{G} \\[1ex]

(\beta.{\rightarrow})
& \expapp{(\expabs{x}{A}{N})}{V}
& \rewrite{2}
& \subst{N}{x}{V} \\

(\beta.{\times_1})
& \expfst{\exppair{V}{W}}
& \rewrite{2}
& V \\

(\beta.{\times_2})
& \expsnd{\exppair{V}{W}}
& \rewrite{2}
& W \\

(\beta.{+_1})
& \expcase{(\expinl{V}{B})}{x}{M}{y}{N}
& \rewrite{2}
& \subst{M}{x}{V} \\

(\beta.{+_2})
& \expcase{(\expinr{A}{W})}{x}{M}{y}{N}
& \rewrite{2}
& \subst{N}{y}{W} \\

(\beta.{\mathit{let}})
& \explet{x}{V}{N}
& \rewrite{2}
& \subst{N}{x}{V} \\
\end{array}
\]

\vspace{2ex}

Phase 3 (garbage collection)
\[
\begin{array}[t]{@@{}llll@@{\quad}l@@{}}

(\mathit{need})
& \explet{x}{\nv}{N}
& \rewrite{3}
& N,
& x \notin \fv{N}\\
%% & \subst{N}{x}{\nv},
%% & Count(x,N) < 2 \\
\end{array}
\]
\caption{Normalisation rules}
\label{fig:norm}
\end{figure*}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This section introduces a collection of reduction rules for
normalising terms that enforces the subformula property
while ensuring sharing is preserved. The rules adapt to
both call-by-need and call-by-value.

We work with simple types. The only polymorphism in our examples
corresponds to instantiating constants (such as $\mathit{while}$) at
different types.

Types, terms, and values are presented in Figure~\ref{fig:term}. We
let $A$, $B$, $C$ range over types, including base types ($\iota$),
functions ($A \to B$), products ($A \times B$), and sums ($A + B$).
We let $L$, $M$, $N$ range over terms, and $x$, $y$, $z$ range over
variables.  We let $c$ range over constants, which are fully
applied.  
We follow the usual convention that terms are
equivalent up to renaming of bound variables. We write $\fv{N}$ for
the set of free variables of $N$, and $\subst{N}{x}{M}$ for
capture-avoiding substitution of $M$ for $x$ in $N$.
%
%% [SL: not relevant]
%% Types correspond to propositions and terms to proofs in
%% minimal propositional logic.
%
We let $V$, $W$ range over values, and $\nv$ range over
terms that are not values.

We let $\Gamma$ range over type environments, which are sets of pairs
of variables with types $x:A$. We write $\Gamma \vdash M:A$ to
indicate that term $M$ has type $A$ under type environment
$\Gamma$. The typing rules are standard.

The grammar of normal forms is given in Figure~\ref{fig:nf}. We reuse
$L,M,N$ to range over terms in normal form and $V,W$ to range over
values in normal form, and we let $Q$ range over neutral forms.

Reduction rules for normalisation are presented in
Figure~\ref{fig:norm}, broken into three phases. We write $M
\mapsto_i N$ to indicate that $M$ reduces to $N$ in phase $i$. We let
$F$ and $G$ range over two different forms of evaluation frame used in
Phases~2 and~3 respectively. We write $\fv{F}$ for the set of free
variables of $F$, and similarly for $G$.
The reduction relation is closed under compatible closure.

\figterm
\fignf
\fignorm
%\figtyping

The normalisation procedure consists of exhaustively applying
the reductions of Phase~1 until no more apply, then similarly for
Phase~2, and finally for Phase~3. Phase~1 performs let-insertion,
naming subterms that are not values.
%% along the lines of a translation to A-normal form
%% \citep{a-normal-form} or reductions (let.1) and (let.2) in Moggi's
%% metalanguage for monads \citep{moggi}.
Phase~2 performs standard $\beta$ and commuting reductions, and is the
only phase that is crucial for obtaining normal forms that satisfy the
subformula property. Phase~3 ``garbage collects'' unused terms, as in
the call-by-need lambda calculus \citep{call-by-need}. Phase~3 may be
omitted if the intended semantics of the target language is
call-by-value rather than call-by-need.

Every term has a normal form.
\begin{proposition}[Strong normalisation]
Each of the reduction relations $\rewrite{i}$ is strongly
normalising: all $\rewrite{i}$ reduction sequences on well-typed
terms are finite.
\end{proposition}
The only non-trivial proof is for $\rewrite{2}$, which can be proved
via a standard reducibility argument (see, for example,
\cite{Lindley07}). If the target language includes general recursion,
normalisation should treat the fixpoint operator as an uninterpreted
constant.

The grammar of Figure~\ref{fig:nf} characterises normal forms
precisely.
\begin{proposition}[Normal form syntax]
\label{prop_normal}
An expression $N$ matches the syntax of normal forms in
Figure~\ref{fig:nf} if and only if it is in normal form with regard to
the reduction rules of Figure~\ref{fig:norm}.
\end{proposition}

[TODO: Give proof hint.]

[TODO: The statement is incorrect. |let x = u v in y z| matches the
grammar, but is not in normal form with regard to $\rewrite{3}$.]

[TODO: Can we delete Proposition~\ref{prop_normal}?]

*** CONTINUE FROM HERE ***

The \emph{subformulas} of a type are the type itself and its
components. For instance, the subformulas of $A \to B$ are itself and
the subformulas of $A$ and $B$. The \emph{proper subformulas} of a
type are all its subformulas other than the type itself.

The \emph{subterms} of term are the term itself and its components.
For instance, the subterms of $\expabs{x}{N}$ are itself and the
subterms of $N$ and the subterms of $\expapp{L}{M}$ are itself and the
subterms of $L$ and $M$.

Constants are always fully applied; they are introduced as a
separate consturct to avoid consideration of irrelevant subformulas
and subterms.
The type of a constant $c$ of arity $k$ is written
\[
c : A_1 \to \cdots A_k \to B
\]
and its subtypes are itself and $A_1$, \ldots, $A_k$, and $B$
(but not $A_i \to \ldots \to A_k \to B$ for $i > 1$).
An application of a constant $c$ of arity $k$ is written
\[
c M_1 \cdots M_k
\]
and its subterms are itself and $M_1$, \ldots, $M_k$
(but not $c M_1 \cdots M_j$ for $j < k$).
Free variables are equivalent to constants of arity zero.

Terms in normal form satisfy the subformula property.

\begin{proposition}[Subformula property]
\label{prop_subformula}
If $\Gamma \vdash M:A$ and the normal form of $M$ is $N$ by the
reduction rules of Figure~\ref{fig:norm}, then $\Gamma \vdash N:A$ and
every subterm of $N$ has a type that is either a subformula of $A$,
a subformula of a type in $\Gamma$, or a subformula of the type
of a constant in $N$.  Further, every subterm other than
$N$ itself and the variables in $\Gamma$ has a type that is
either a proper subformula of $A$,
a proper subformula of a type in $\Gamma$,
or a proper subformula of the type of a constant in $N$.
\end{proposition}

\section{Other examples of QDSLs}
\label{sec:other-qdsls}

\subsection{F\# P-LINQ}
\label{sec:linq}

\subsection{Scala LMS}
\label{sec:lms}

\section{A comparison of QDSL and EDSL}
\label{sec:qdsl-vs-edsl}

%include Section2.lhs
%include Section3.lhs

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


