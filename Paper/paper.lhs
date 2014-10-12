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
%format x0
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
\newcommand{\rmdiv}{\mathbin{\textrm{div}}}

\newcommand{\citet}[1]{\cite{#1}}
\newcommand{\citep}[1]{(\cite{#1})}

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

\subsection{First example}
\label{sec:first-example}

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
argument, where |C| is a type that represents |C| code.
Here is a solution to our problem using EDSL.
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
run-time, using |M == N| and |if L then M else N| for the former but
|M .==. N| and |L ?  (M, N)| for the latter.

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
\end{code}
Here |sqr| is as before. The above uses
\begin{code}
data Maybe a = Nothing | Just a
return :: a -> Maybe a
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
maybe :: b -> (a -> b) -> Maybe a -> b
\end{code}
from the Haskell prelude. Type |Maybe| is declared as a monad, enabling the |do| notation,
which translates into |(>>=)|. 
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
power'' n x  = option 0 (\y -> y) (power' n x)
\end{code}
Here |sqr| is as before. The above uses the functions
\begin{code}
none   :: Option a
return :: a -> Option a
(>>=)  :: Option a -> (a -> Option b) -> Option b
option :: (Syntactic a, Syntactic b) => b -> (a -> b) -> Option a -> b
\end{code}
from the EDSL library. Details of the type |Option| and the type class |Syntactic|
are explained in Section~\ref{sec:option}.
Type |Option| is declared as a monad, enabling the |do| notation,
which translates into |(>>=)|.
Invoking |edslC (power'' (-6))| generates the same C code as
the previous example.

In order to be easily represented in C, type |Option a| is represented as a pair
consisting of a boolean and the representation of the type |a|; in the case that
corresponds to |Nothing|, the boolean is false and a default value of type |a|
is provided.  The EDSL term generated by evaluating |power (-6) 0| is large and
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
Section~\ref{sec:option}.)  Before, evaluating |power| yielded an EDSL term
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
Here |L|,|M|,|N|,|P|,|Q| range over |EDSL| terms, and
|M[L:=P]| stands for |M| with each occurence of |L| replaced by |P|.
After applying these rules, common subexpression
elimination yields the same structure as in the previous subsection,
from which the same C code is generated.

Hence, an advantages of the EDSL approach---that it generates
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
power' :: Int -> QDSL (Float -> Maybe Float)
power' n x  =
  if n < 0 then
    <|| if x == 0 then Nothing else do y <- $(power' (-n) x); return (1 / y) ||>
  else if n == 0 then
    <|| return 1 ||>
  else if even n then 
    <|| do y <- $(power' (n `div` 2) x); return $(sqr y) ||>
  else
    <|| do y <- $(power' (n-1) x); return (x * y) ||>

power'' :: Int -> QDSL (Float -> Float)
power'' n x  =  <|| maybe 0 (\x -> x) $(power' n x) ||>
\end{code}
Here |sqr| is as before,
and |Nothing|, |return|, |(>>=)|, and |maybe| are typed as before,
and provided for use in quoted terms by the QDSL library.

Evaluating |QDSL (powerQ'' (-6))| yields a term of similar complexity
to the term yielded by the EDSL. Normalisation by the rules discussed
in Section~\ref{sec:normalise} reduces the term to the same form
as before, which in turn generates the same C as before.  

Here are some further points of comparison between the two approaches.
\begin{itemize}

\item Both EDSL and QDSL can exploit notational conveniences in the
host language. The example here exploits Haskell |do| notation; the
embedding SQL in F\# by \citet{CheneyLW13} expoited F\# sequence
notation. For the EDSL, exploiting |do| notation just requires
instantiating |return| and |(>>=)| correctly. For the QDSL, it is
also necessary for the normaliser to recognise and expand
|do| notation and to substitute appropriate instances of 
|return| and |(>>=)|.

\item As this example shows, sometimes both EDSLs and QDSLs
may require normalisation. Each EDSL usually has a distinct
deep representation and so requires a distinct normaliser.
In contrast, all QDSLs can share the representation of the
quoted host language, and so can share a normaliser.

\end{itemize}


\section{Combining deep and shallow embedding}

We now review the usual approach to embedding a DSL into
a host language by combining deep and shallow embedding.
As a running example, we will use MiniFeldspar, an embedded DSL for
generating signal processing software in C.
Much of this section reprises \citet{SvenningssonA12}.

\subsection{The deep embedding}

Recall that a value of type |EDSL a| represents a term of type |a|,
and is called a deep embedding.  In what follows, we shorten |EDSL a|
to |E a|.
\begin{code}
data E a where
  LitB		:: Bool -> E Bool
  LitI		:: Int -> E Int
  LitF		:: Float -> E Float
  If		:: E Bool -> E a -> E a -> E a
  While		:: (E a -> E Bool) -> (E a -> E a) -> E a -> E a
  Pair		:: E a -> E b -> E (a,b)
  Fst		:: E (a,b) -> E a
  Snd           :: E (a,b) -> E b
  Prim1		:: String -> (a -> b) -> E a -> E b
  Prim2         :: String -> (a -> b -> c) -> E a -> E b -> E c
  Variable      :: String -> E a
  Value         :: a -> E a
\end{code}
The type above represents a low level, pure functional language
with a straightforward translation to C. It uses higher-order
abstract syntax (HOAS) to represent constructs with variable binding
\citet{hoas}.

Our EDSL has boolean, integer, and floating point literals,
conditionals, while loops, pairs, primitives,
and special-purpose constructs for variables and values.
(Later we will add constructs for arrays and for a default value.)
Rather than using side-effects, the while loop takes
three arguments, a function from current state |a| to a boolean,
and a function from current state |a| to new state |a|,
and initial state |a|, and returns final state |a|.
Constructs |Prim1| and |Prim2| represent primitive
operations, the string is the name of the operation
(used in printing or to generate C) and the function
argument computes the primitive (used in evaluation).
Construct |Variable| is used in printing and in generating C,
construct |Value| is used in the evaluator.

The exact semantics is given by |eval|. It is a strict language,
so we define an infix strict application operator |(<*>)|.
\begin{code}
(<*>)                 :: (a -> b) -> a -> b
f x                   =  seq x (f x)

eval                  :: E a -> a
eval (LitI i)         =  i
eval (LitF x)         =  x
eval (LitB b)         =  b
eval (If c t e)       =  if eval c then eval t else eval e
eval (While c b i)    =  evalWhile (evalFun c) (evalFun b) <*> eval i
eval (Pair a b)       =  (,) <*> eval a <*> eval b
eval (Fst p)          =  fst <*> eval p
eval (Snd p)          =  snd <*> eval p
eval (Prim1 _ f a)    =  f <*> eval a
eval (Prim2 _ f a b)  =  f <*> eval a <*> eval b
eval (Value a)        =  a

evalFun               :: (E a -> E b) -> a -> b
evalFun f x           =  (eval . f . Value) <*> x

evalWhile             :: (a -> Bool) -> (a -> a) -> a -> a
evalWhile c b i       =  if c i then evalWhile c b (b i) else i
\end{code}
Function |eval| plays no role in generating C, but may be useful for testing.


\subsection{Class |Syntactic|}

We introduce a type class |Syntactic| that allows us to convert
between types in the host language, Haskell, and corresponding
representations in the target languagee, the deep embedding |E|.
\begin{code}
class Syntactic a where
  type Internal a
  toE    :: a -> E (Internal a)
  fromE  :: E (Internal a) -> a
\end{code}
The type families feature of GHC is used to specify for each
type |a| in the host language a corresponding representation |Internal a|
in the target language \cite{type-families}. Functions |toE| and
|fromE| translate between the host representation |a| and the
target representation |E (Internal a)|.

The first instance of |Syntactic| is |E| itself, and is straightforward.
\begin{code}
instance Syntactic (E a) where
  type Internal (E a) = a
  toE    =  id
  fromE  =  id
\end{code}
This instance is used for base types. Our representation of a run-time
|Bool| will have type |E Bool| in both the host and target
languages, and similarly for |Int| and |Float|.

We do not code the target language using its constructors
directly. Instead, for each constructor we define a corresponding
``smart constructor'' using class |Syntactic|.
\begin{code}
true, false  :: E Bool
true         =  LitB True
false        =  LitB False

(?)          :: Syntactic a => E Bool -> (a,a) -> a
c ? (t,e)    =  fromE (If c (toE t) (toE e))

while        :: Syntactic a => (a -> E Bool) -> (a -> a) -> a -> a
while c b i  =  fromE (While (c . fromE) (toE . b . fromE) (toE i))
\end{code}

Numbers are made convenient to manipulate via overloading.
\begin{code}
instance Num (FunC Int) where
  a + b          =  Prim2 "(+)" (+) a b 
  a - b          =  Prim2 "(-)" (-) a b 
  a * b          =  Prim2 "(*)" (*) a b
  fromInteger a  =  LitI (fromInteger a)
\end{code}
With this declaration, |1+2 :: E Int| evaluates to
|Prim2 "(+)" (+) (LitI 1) (LitI 2)|, permitting code
executed at generation-time and run-time to appear identical.
A similar declaration works for |Float|.

Comparison also benefits from a smart constructor.
\begin{code}
(.==.)       :: (Syntactic a, Eq (Internal a)) => a -> a -> E Bool
a .==. b     =  Prim2 "(==)" (==) (toE a) (toE b)
\end{code}
Overloading cannot apply here, because Haskell requires
|(==)| return a result of type |Bool|, while |(.==.)| returns
a result of type |E Bool|.  A similar declaration works for |(.<.)|.


\subsection{Embedding pairs}

We set up a correspondence between pairs in the
host language and pairs in the target language.
\begin{code}
instance (Syntactic a, Syntactic b) where
  type  Internal (a,b)  =  (Internal a, Internal b)
  toE (a,b)             =  Pair (toE a, toE b)
  fromE p               =  (fromE (Fst p), fromE (Snd p))
\end{code}
This permits us to manipulate pairs in the host language as normal,
with |(a,b)|, |fst a|, and |snd a|, using class |Syntactic| to convert
to the target language as necessary.  Argument |p| is duplicated in
the definition of |fromE|, which may require common
subexpression elimination or observable sharing, as discussed in
Section~\ref{sec:first-example}.

We have now developed sufficient machinery to define a |for| loop
in terms of a |while| loop.
\begin{code}
for :: Syntactic a => E Int -> a -> (E Int -> a -> a) -> a
for n x0 b  =  snd (while (\(i,x) -> i .<. n) (\(i,x) -> (i+1, b i x)) (0,x0))
\end{code}
The state of the |while| loop is a pair consisting of a counter and
the state of the |for| loop. The body |b| of the |for| loop is a function
that expects both the counter and the state of the |for| loop.
The counter is discarded when the loop is complete, and the final state
of the |for| loop returned.

Thanks to our machinery, the above definition uses only ordinary Haskell
pairs. The condition and body of the |while| loop pattern match on the
state using ordinary pair syntax, and the initial state is constructed
as a standard Haskell pair.


\subsection{Embedding undefined}

For the next section, which defines an analogue of the |Maybe| type, it
will prove convenient to work with types which have a distinguished
value at each type, which we call |undef|. (Better names are `default'
or `undefined', but both already have other meanings in Haskell.)

It is straightforward to define a type class |Undef|, where type |a|
belongs to |Undef| if it belongs to |Syntactic| and it has an
undefined value.
\begin{code}
class Syntactic a => Undef a where
  undef :: a

instance Undef (E Bool) where
  undef = false

instance Undef (E Int) where
  undef = 0

instance Undef (E Float) where
  undef = 0

instance (Undef a, Undef b) => Undef (a,b) where
  undef = (undef, undef)
\end{code}

For example,
\begin{code}
(/#) :: E Float -> E Float -> F Float
x /# y  =  (y .==. 0) ? (undef, x/y)
\end{code}
behaves as division, save that when the divisor is zero
it returns the undefined value of type |Float|, which
is also zero.

\citet{SvenningssonA12} claim that it is not possible
to support |undef| without modifying the type |E|, but they
appear to have underestimated the power of their own technique.


\subsection{Embedding option}

We now explain in detail the |Option| type seen in Section~\ref{sec:second-example}.

The deep-and-shallow technique cleverly represents pairs in the target
with pairs in the host. Alas, we cannot use the same trick for |Maybe|,
because |fromE| would have to decide at generation-time whether to return
|Just| or |Nothing|, but which is correct is not known until run-time.

One could extend type |E| to also represent values of |Maybe| type,
but \citet{SvenningssonA12} prefer to leave |E| minimal. Following
their development, corresponding to |Maybe a| we introduce a type
|Opt_R a|, which consists of a boolean and a value of type |a|.
Corresponding to |Just x|, the boolean is true and the value is |x|,
corresponding to |Nothing|, the boolean is false and the value is
|undef|.  We take |some_R|, |none_R|, and |opt_R| as the analogues of
|Just|, |Nothing|, and |maybe|.  The |Syntactic| instance is
straightforward, mapping options to and from the pairs already defined
for |E|.
\begin{code}
data Opt_R a = Opt_R { def :: E Bool, val :: a }

instance Syntactic a => Syntactic (Opt_R a) where
  type Internal (Opt_R a)  =  (Bool, Internal a)
  toE (Opt_R b x)          =  Pair b (toE x)
  fromE p                  =  Opt_R (Fst p) (fromE (Snd p))

some_R         :: a -> Opt_R a
some_R x       =  Opt_R true x

none_R         :: Undef a => Opt_R a
none_R         =  Opt_R false undef

opt_R          :: Syntactic b => b -> (a -> b) -> Opt_R a -> b
opt_R d f o    =  def o ? (f (val o), d)
\end{code}

The next obvious step is to define a suitable monad over the type |Opt_R|.
The natural definitions to use are:
\begin{code}
return    :: a -> Opt_R a
return x  =  some_R x

(>>=)     :: (Undef b) => Opt_R a -> (a -> Opt_R b) -> Opt_R b
o >>= g   =  Opt_R  (def o ? (def (g (val o)), false))
                    (def o ? (val (g (val o)), undef))
\end{code}
but this requires the type constraint |Undef b| which is not part of
the type of |(>>=)|. This is a common issue defining monads, called
the constrained-monad problem 
\citet{hughes:restricted-monad,SculthorpeBGG13}.
To solve it, we follow a trick due to \cite{PerssonAS11}.

We introduce a second continuation-passing style (cps) type |Opt|,
defined in terms of the representation type |Opt_R|.  It is
straightforward to define |Monad| and |Syntax| instances for the cps
type, operations to lift the representation type to cps and to lower
cps to the representation type, and to lift |some|, |none|, and
|option| from the representation type to the cps type.
The |lift| operation is closely related to the |(>>=)| operation
we could not define above; it is properly typed,
thanks to the type constraint on |b| in the definition of |Opt a|.

\todo{fix typesetting of dot after $\forall$}
\begin{code}
newtype Opt a = O { unO :: forall b . Undef b => ((a -> Opt_R b) -> Opt_R b) }

instance Monad Opt where
  return x    = O (\g -> g x)
  m >>= k     = O (\g -> unO m (\x -> unO (k x) g))

instance Undef a => Syntactic (Opt a) where
  type Internal (Opt a) = (Bool, Internal a)
  fromE = lift . fromE
  toE   = toE . lower

lift          :: Opt_R a -> Opt a
lift o        =  O (\g -> Opt_R  (def o ? (def (g (val o)), false))
                                 (def o ? (val (g (val o)), undef)))

lower         :: Undef a => Opt a -> Opt_R a
lower m       =  unO m some_R

some          :: a -> Opt a
some a        =  lift (some_R a)

none          :: Undef a => Opt a
none          =  lift none_R

option        :: (Undef a, Undef b) => b -> (a -> b) -> Opt a -> b
option d f o  =  option_R d f (lower o) 
\end{code}

These definitions are adequate to support the EDSL code presented
in Section~\ref{sec:second-example}.

\todo{After discussing the subformula property, a different solution
to this problem becomes available.}


\subsection{Embedding Vector}



%   Arr           :: E Int -> (E Int -> E a) -> E (Array Int a)
%   ArrLen        :: E (Array Int a) -> E Int
%   ArrIx         :: E (Array Int a) -> E Int -> E a


% eval (Arr n f)        =  array (0,n') [ (i, eval (f (LitI i))) | i <- [0..n'] ]
%                          where n' = eval n - 1
% eval (ArrLen a)       =  u - l + 1  where (l,u) = bounds (eval a)
% eval (ArrIx a i)      =  eval a ! eval i


\section{The subformula property}


\section{The design of QuickDSL}


\section{Empirical results}


This is a test.

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

