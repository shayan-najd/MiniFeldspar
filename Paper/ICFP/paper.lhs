% TO DO

% Shayan, I especially need the following from you. Either send in
% e-mail or edit the paper. If you edit the paper, mark changes with
% *** SHAYAN ***
% before and after.

% Shayan: Add a short explanation of the type class Type. In order for
% the code in the paper to be correct, I suspect that we need to explain
% how Type relates to Ref.

% Shayan: Check that all the C code in the paper is indeed generated
% by the code given in the paper. Particularly, check this for Section
% 2.6, where the C code is new.

% Shayan: A short explanation of how overloading works in the QDSL
% Feldspar implementation.


% Possible additional things to do?

% Possibly, introduce class Type and explain how it relates
% to Rep?  If done, may need to move some explanation of Arr
% from 2.6 to 2.1.

% Possibly, eliminate non-CSE results from Figure 1?


% Partly done: should I do more?

% More discussion of Cheney-et-al-2013
% especially in regard to subformula property.

% More discussion of type-based quotation

% Make more clear that main contribution
% is the application of subformula property to DSLs.


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
%format Opt_R = Opt'
%format some_R = some'
%format none_R = none'
%format opt_R = opt'
%format option_R = option'
%format ^ = " "
%format ... = "\cdots"
%format s_0
%format A_1
%format A_k

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

\newcommand{\todo}[1]
  {{\noindent\small\color{red}
   \framebox{\parbox{\dimexpr\linewidth-2\fboxsep-2\fboxrule}{\textbf{TODO:} #1}}}}
%\newcommand{\todo}[1]{}

\newcommand{\longeq}{=\!=}
% \newcommand{\openq}{[||||\,}
% \newcommand{\closeq}{\,||||]}
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

\newcommand{\flushr}{{}\mbox{~}\hfill}%{\flushright\vspace{-2ex}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
\newcommand{\expabs}[3]{\lambda#1.\,#3}
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
       \expconst{c}{(\overline{V},\hole,\overline{N})}  \mid
       \expapp{\hole}{M}                                \mid
       \expapp{V}{\hole}                                \mid
       \exppair{\hole}{M}                               \mid
       \exppair{V}{\hole}                               \mid
       \expfst{\hole}                                   \mid
       \expsnd{\hole}                                   \mid % \\ &&
       \expinl{\hole}{B}                                \mid
       \expinr{A}{\hole}                                \mid
       \expcase{\hole}{x}{M}{y}{N} \\
 \end{array}
\]%
\[
\begin{array}{lllll}
(\mathit{let})
& F[\nv]
& \rewrite{1}
& \explet{x}{\nv}{F[x]},
& x \text{ fresh}
\end{array}
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

We describe a new approach to domain specific languages (DSLs), called
Quoted DSLs (QDSLs), that resurrects two old ideas: quotation, from
McCarthy's Lisp of 1960, and the subformula property, from Gentzen's
natural deduction of 1935.  Quoted terms allow the DSL
to share the syntax and type system of the host language.
Normalising quoted terms ensures the subformula property, which
guarantees that one can use higher-order or nested types in the source while
guaranteeing first-order or flat types in the target, and enables using types to
guide fusion.  We test our ideas by re-implementing Feldspar, which
was originally implemented as an Embedded DSL (EDSL), as a QDSL; and
we compare the QDSL and EDSL variants.

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
When everything old is new again \\
\flushr -- Peter Allen and Carole Sager
\end{quote}

Implementing domain-specific languages (DSLs) via quotation is one of
the oldest ideas in computing, going back at least to McCarthy's Lisp,
which was introduced in 1960 and had macros as early as 1963.  Today,
a more fashionable technique is Embdedded DSLs (EDSLs), which may use
shallow embedding, deep embedding, or a combination of the two. Our
goal in this paper is to reinvigorate the idea of building DSLs via
quotation, by introducing a new approach which we dub Quoted DSLs
(QDSLs).  A key feature of QDSLs is the use of normalisation to ensure
the subformula property, first proposed by Gentzen in 1935.

\vspace{2ex}
\begin{quote}
Imitation is the sincerest of flattery. \\
\flushr --- Charles Caleb Colton
\end{quote}

\citet{cheney:linq} describes a DSL for language-integrated query in
F\# that translates into SQL.  Their technique depends on quotation,
normalisation of quoted terms, and the subformula property---an
approach which we here dub QDSL.  They conjecture that other DSLs might
benefit from the same technique, particularly those that perform
staged computation, where host code at generation-time computes target
code to be executed at run-time.

Generality starts at two.  Here we test the conjecture of
\citet{cheney:linq} by reimplementing the EDSL Feldspar
\citep{FELDSPAR} as a QDSL.  We describe the key features of the
design, and show that the performance of the two versions is
comparable.  We compare the QDSL and EDSL variants of Feldspar, and
assess the tradeoffs between the two approaches.

\citet{Davies-Pfenning-1996,Davies-Pfenning-2001} also suggest
quotation as a foundation for staged computation, and note
a propositions-as-types connection between quotation and
a modal logic; our type |Qt a| corresponds to their
type $\bigcirc a$. They also mention in passing the utility of
normalising quoted terms, although they do not mention the
subformula property.

The .NET Language-Integrated Query (LINQ) framework as used in C\# and
F\# \citep{csharplinq,fsharplinq}, and the Lightweight Modular Staging
(LMS) framework as used in Scala \citep{scalalms}, exhibit
considerable overlap with the techniques described here.  Notably,
they use quotation to represent staged DSL programs, and they make use
to a greater or lesser extent of normalisation.  In F\# LINQ quotation
is indicated in the normal way (by writing quoted programs inside
special symbols), while in C\# LINQ and Scala LMS quotation is
indicated by type inference (quoted terms are given a special type).

In short, some researchers and developers are already exploiting this
technique. We propose QDSL as a name to capture the commonalities
among these approaches, and we observe the utility of the subformula
property in this context.

\vspace{2ex}
\begin{quote}
Perhaps we may express the essential properties of such a normal proof
by saying: it is not roundabout. \\
\flushr --- Gerhard Gentzen
\end{quote}

Our approach exploits the fact that normalised terms satisfy the
subformula property, first introduced in the context
of natural deduction by \citet{Gentzen-1935},
and improved by \citet{Prawitz-1965}.

The subformulas of a formula are its subparts;
for instance, the subformulas of |A -> B| are the formula
itself and the subformulas of |A| and |B|.
The subformula property states that every proof can be put
into a normal form where the only propositions that appear
in the proof are subformulas of the hypotheses and conclusion
of the proof. Applying the principle of Propositions as Types
\citep{Howard-1980,Wadler-2015}, the subformula property states that every
lambda term can be put into a normal form where the only
types that appear in the term are subformulas of the
types of the free variables and the type of the term itself.

The subformula property provides users of the
DSL with useful guarantees, such as the following:
\begin{itemize}

\item they may write higher-order terms
while guaranteeing to generate first-order code;

\item they may write a sequence of loops over arrays
while guaranteeing to generate code that fuses those loops;

\item they may write intermediate terms of nested type
while guaranteeing to generate code that operates on flat data.

\end{itemize}
The first two of these are used in this paper, while the
third is central to \citet{cheney:linq}.
% We thus give modern application to a theorem four-fifths of a century old.

The subformula property is closely related to conservativity.  A
conservativity result expresses that adding a feature to a system of
logic, or to a programming language, does not make it more expressive.
Consider intuitionistic logic with conjunction; conservativity states
that adding implication to this logic proves no additional theorems
that can be stated in the original logic. Such a conservativity result
is an immediate consequence of the subformula property; since the
hypotheses and conjuction of the proof only mention conjunction, any
proof, even if it uses implication, can be put into a normal form that
only uses conjunction. Equivalently, any lambda calculus term that
mentions only pair types in its free variables and result, even if it
uses functions, can be put in a normal form that only uses pairs. Such
a result is related to the first bullet point above; see
Proposition~\ref{prop:rank} in Section~\ref{sec:subformula}.

As another example, the third bullet point above corresponds to a standard
conservativity result for databases, namely that nested queries are no
more expressive than flat queries \citep{Wong-1993}.  This conservativity
result, as implied by the subformula property, is used central to
\citep{cheney:linq} to show that queries that use intermediate nesting
can be translated to SQL, which only queries flat tables and does not
support nesting of data.

The subformula property holds only for terms in normal form.  Previous
work, such as \citet{cheney:linq} uses a call-by-name normalisation
algorithm that performs full $\beta$-reduction, which may cause
computations to be repeated.  Here we present call-by-value and
call-by-need normalisation algorithms, which guarantee to preserve
sharing of computations. We also present a sharpened version of
the subformula property, which we apply to characterise
the circumstances under which a QDSL may guarantee
to generate first-order code.

\vspace{2ex}
\begin{quote}
Good artists copy, great artists steal. \flushr --- Picasso
\end{quote}

EDSL is great in part because it steals the type system of its host
language. Arguably, QDSL is greater because it steals the type system,
the syntax, and the normalisation rules of its host language.

In theory, an EDSL should also steal the syntax of its host language,
but in practice the theft is often only partial.
For instance, an EDSL such as Feldspar or Nicola,
when embedded in Haskell, can exploit the overloading of Haskell so that
arithmetic operations in both languages appear identical, but the same
is not true of comparison or conditionals. In QDSL, of necessity the
syntax of the host and embedded languages must be identical. For
instance, this paper presents a QDSL variant of Feldspar, again in
Haskell, where arithmetic, comparison, and conditionals are all
represented by quoted terms, and hence identical to the host.

In theory, an EDSL also steals the normalisation rules of its host
language, by using evaluation in the host to normalise terms of the
target. In Section~\ref{sec:qdsl-vs-edsl} we give several examples
comparing our QDSL and EDSL versions of Feldspar. In the first of
these, it is indeed the case that the EDSL achieves by evaluation of
host terms what the QDSL achieves by normalisation of quoted terms.
However, in other cases, the EDSL must perform normalisation of the
deep embedding corresponding to what the QDSL achieves by
normalisation of quoted terms.

\vspace{2ex}
\begin{quote}
Try to give all of the information to help others to judge the value
of your contribution; not just the information that leads to judgment
in one particular direction or another. \\
\flushr --- Richard Feynman
\end{quote}

The subformula property depends on normalisation, but normalisation
may lead to an exponential blowup in the size of the normalised
code. In particular, this occurs when there are nested conditional or
case statements. We explain how the QDSL technique can offer the user
control over where normalisation does and does not occur, while still
maintaining the subformula property. Future work is required to
consider the trade-offs between full normalisation as required for
the subformula property and special-purpsose normalisation as used
in most DSLs; possibly a combination of both will prove fruitful.

Some researchers contend that an essential property of an embedded DSL
which generates target code is that every term that is type-correct
should successfully generate code in the target language. Neither the
P-LINK of \citet{cheney:linq} nor the QDSL Feldspar of this paper
satisfy this property, since the user is required to eyeball quoted code
to ensure it mentions only permitted operators. If this is thought too
onerous, it is possible to ensure the property with additional preprocessing.

\vspace{2ex}
\begin{quote}
This is the short and the long of it. \flushr --- Shakespeare
\end{quote}

The contributions of this paper are:
\begin{itemize}

\item To introduce QDSLs as an approach to building DSLs based
on quotation, normalisation of quoted terms, and the subformula property
by presenting the design of a QDSL variant of Feldspar. (Section~\ref{sec:qfeldspar}.)

\item To compare QDSL and EDSL implementations of Feldspar,
and show that they offer comparable performace.
(Section~\ref{sec:implementation}.)

\item To explain the role of the subformula property in formulating
DSLs, to describe a normalisation algorithm suitable for call-by-value
or call-by-need that does not lose sharing, and to formulate a
sharpened version of the subformula property and apply it to
characterise when higher-order terms normalise to first-order form
(Section~\ref{sec:subformula}.)

% \item To review the F\# implementation of language-integrated query
% \citep{cheney:linq} and the Scala LMS implementations of query
% and [TODO: what else?], and argue that these are instances of QDSL.
% (Section~\ref{sec:other-qdsls}.)

\item To compare the QDSL variant of Feldspar
with the deep and shallow embedding approach
used in the EDSL variant of Feldspar.
(Section~\ref{sec:qdsl-vs-edsl}.)

\end{itemize}
Section~\ref{sec:related} describes related work, and
Section~\ref{sec:conclusion} concludes.

% \section{A QDSL variant of Feldspar}
% \label{sec:qfeldspar}

% \section{The subformula property}
% \label{sec:subformula}

% \section{Implementation}
% \label{sec:implementation}

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


\section{Feldspar as a QDSL}
\label{sec:qfeldspar}

Feldspar is an EDSL for writing signal-processing software, that
generates code in C \citep{FELDSPAR}. We present a variant,
QDSL~Feldspar, that follows the structure of the previous design closely,
but using the methods of QDSL rather than EDSL. We make a detailed
comparison of the QDSL and EDSL designs in Section~\ref{sec:qdsl-vs-edsl}.

% \subsection{Design}
% \label{subsec:qfeldspar-design}

\subsection{The top level}
\label{subsec:top}

In QDSL~Feldspar, our goal is to translate a quoted term to C code.
The top-level function has the type:
\begin{spec}
qdsl :: (Rep a , Rep b) => Qt (a -> b) -> C
\end{spec}
Here |Qt a| represents a Haskell term of type |a|, its
\emph{quoted} representation, and type |C| represents code in C.
The top-level function expects a quoted term representing
a function from type |a| to type |b|, and returns C code
that represents this function (a @main@ routine).

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

\subsection{A first example}
\label{subsec:power}

Let's begin by considering the ``hello world'' of program generation,
the power function, raising a float to a given integer.
Since division by zero is undefined, we
arbitrarily choose that raising zero to a negative power yields zero.
Here is the power function represented using QDSL:
\begin{code}
power :: Int -> Qt (Float -> Float)
power n =
  if n < 0 then
    [|| \x -> if x == 0  then  0
                         else  1 / ($$(power (-n)) x) ||]
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
[||  \x ->  if x == 0 then 0 else
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
explain below why this is guaranteed by the subformula property.
From the normalised term it is easy to generate the final C code:
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
undesirable because it is too noisy.   QDSL enables us to ``steal'' the entire
syntax of the host language for our DSL.  The EDSL approach can use
the same syntax for arithmetic operators, but must use a different
syntax for equality tests and conditionals, as we explain in
Section~\ref{sec:qdsl-vs-edsl}.

Within the quotation brackets there appear lambda abstractions and
function applications, while our intention is to generate first-order
code. How can the QDSL~Feldspar user be certain that such function
applications do not render transformation to first-order code
impossible or introduce additional runtime overhead?  The answer is
the subformula property.


\subsection{The subformula property}
\label{subsec:subformula}

Gentzen's subformula property guarantees that any proof can be
normalised so that the only formulas that appear within it are
subformulas of one of the hypotheses or of the conclusion of the
proof.  Viewed through the lens of Propositions as Types
\citep{Howard-1980,Wadler-2015}, also known as the Curry-Howard
Isomorphism, Gentzen's subformula property guarantees that any term
can be normalised so that the type of each of its subterms is a
subtype of either the type of one of its free variables (corresponding
to hypotheses) or the term itself (corresponding to the conclusion).
Here the subtypes of a type are the type itself and the subtypes of
its parts, where the parts of |a -> b| are |a| and |b|, the parts of
|(a,b)| are |a| and |b|, and that types |Int| and |Float| have no
parts.

% the only part of |Arr a| is |a|

Further, it is easy to sharpen Gentzen's proof to guarantee a
sharpened subformula property: any term can be normalised so that the
type of each of its proper subterms is a proper subtype of either the
type of one of its free variables (corresponding to hypotheses) or the
term itself (corresponding to the conclusion).  Here the proper
subterms of a term are all subterms save for free variables and the
term itself, and the proper subformulas of a type are all subformulas save
for the type itself.  In the example of the previous subsection, the
sharpened subformula property guarantees that after normalisation a
term of type |float -> float| will only have proper subterms of type
|float|, which is indeed true for the normalised term.

The subformula property depends on normalisation of terms, but
complete normalisation is not always possible or desirable.  The
extent of normalisation may be controlled by introducing uninterpreted
constants.  In a context with recursion, we take |fix :: (a -> a) ->
a| as an uninterpreted constant.  In a context where we wish to avoid
unfolding a reduction |L M|, we take |id :: a -> a| as an
uninterpreted constant, and replace |L M| by |id L M|.

% (Careful readers will have noticed a small difficulty.  One of the
% free variables of our quoted term is multiplication over floats.  In
% Haskell, |m*n| abbreviates |(((*) m) n)|, which has |((*) m)| as a
% subterm, and the type of |(*)| is |(float -> (float -> float))|, which has
% |(float -> float)| as a subtype. We alleviate the difficulty by a
% standard trick: each free variable is assigned an arity and must
% always be fully applied. Taking |(*)| to have arity 2 requires we
% always write |m*n| in our code. Then we may, as natural, regard |m|
% and |n| as the only subterms of |m*n|, and |float| as the only subtype
% of the type of |(*)|. Details appear in Section~\ref{sec:subformula}.)

\subsection{A second example}
\label{subsec:maybe}

In the previous code, we arbitrarily chose that raising zero to a
negative power yields zero. Say that we wish to exploit the |Maybe|
type of Haskell to refactor the code, separating identifying the
exceptional case (negative exponent of zero) from choosing a value for
this case (zero).  We decompose |power| into two functions |power'|
and |power''|, where the first returns |Nothing| in the exceptional
case, and the second maps |Nothing| to a suitable default value.

The |Maybe| type is a part of the Haskell standard prelude.
\begin{spec}
data Maybe a = Nothing | Just a
maybe   ::  b -> (a -> b) -> Maybe a -> b
return  ::  a -> Maybe a
(>>=)   ::  Maybe a -> (a -> Maybe b) -> Maybe b
\end{spec}

Here is the refactored code.
\begin{code}
power' :: Int -> Qt (Float -> Maybe Float)
power' n =
  if n < 0 then
    [|| \x ->  if x == 0  then  Nothing
                          else  do  y <- $$(power' (-n)) x
                                    return (1 / y) ||]
  else if n == 0 then
    [|| \x -> return 1 ||]
  else if even n then
    [|| \x -> do  y <- $$(power' (n `div` 2)) x
                  return ($$sqr y) ||]
  else
    [|| \x -> do  y <- $$(power' (n-1)) x
                  return (x*y) ||]

power'' ::  Int -> Qt (Float -> Float)
power'' n =
  [|| \ x ->  maybe 0 (\y -> y) ($$(power' n) x)||]
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
Rather than taking |return|, |(>>=)|, and |maybe| as free variables
(whose types have subformulas involving |Maybe|),
we treat them as known definitions to be eliminated by the
normaliser.
The |Maybe| type is essentially a sum type, and normalisation for
these is as described in Section~\ref{sec:subformula}.

% We have chosen not to make |Maybe| a representable type, which
% prohibits its use as argument or result of the top-level function
% passed to |qdsl|. An alternative choice is possible, as we will see
% when we consider arrays, in Section~\ref{subsec:arrays} below.


\subsection{While}
\label{subsec:while}

Code that is intended to compile to a @while@ loop in C is indicated
in QDSL~Feldspar by application of |while|.
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
fib =  [|| \n -> fst ($$for n (\(a,b) -> (b,a+b)) (0,1)) |]]
\end{code}

Again, the subformula property plays a key role.
As explained in Section~\ref{subsec:subformula}, primitives of the
language to be compiled, such as |(*)| and |while|, are treated as
free variables of a given arity.
As described in Section~\ref{sec:subformula},
we can ensure that after normalisation every occurence of |while|
has the form
\begin{spec}
while (\s -> ...) (\s -> ...) (...)
\end{spec}
where the first ellipses has type |Bool|,
and both occurrences of the bound variable |s|
and the second and third ellipses all have the same type.

Unsurprisingly, and in accord with the subformula property, each
occurrence of |while| in the normalised code will contain subterms
with the type of its state. The restriction of state to representable
types increases the utility of the subformula property. For instance,
since we have chosen that |Maybe| is not a representable type, we can
ensure that any top-level function without |Maybe| in its type will
normalise to code not containing |Maybe| in the type of any subterm.
An alternative choice is possible, as we will see in the next section.


\subsection{Arrays}
\label{subsec:arrays}

A key feature of Feldspar is its distinction between two types of
arrays, manifest arrays, |Arr|, which may appear at run-time, and
``pull arrays'', |Vec|, which are eliminated by fusion at generation-time.
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
toVec        ::  Qt (Arr a -> Vec a)
toVec        =   [|| \a -> Vec (lenArr a) (\i -> ixArr a i) ||]

fromVec      ::  Qt (Vec a -> Arr a)
fromVec      =   [|| \(Vec n g) -> makeArr n (\ x -> g x) ||]
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
normVec  =   [|| \v -> sqrt ($$scalarProd v v) ||]
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

Types and the subformula property help us to guarantee fusion.
The subformula property guarantees that all occurrences
of |Vec| must be eliminated, while occurrences of |Arr| will remain.
There are some situations where fusion is not beneficial, notably
when an intermediate vector is accessed many times fusion will cause
the elements to be recomputed.  An alternative is to materialise the
vector as an array with the following function.
\begin{code}
memorise  ::  Syn a => Qt (Vec a -> Vec a)
memorise  =   [|| toVec . fromVec ||]
\end{code}
For example, if
\begin{spec}
blur :: Rep a => Qt (Vec a -> Vec a)
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

We have described the application of the subformula to array
fusion as based on ``pull arrays'' \citep{svenningsson:combining},
but the same technique should also apply to other techniques that
support array fusion, such as ``push arrays'' \citep{claessen:push}.

\section{Implementation}
\label{sec:implementation}

\input{table}

The original EDSL~Feldspar generates values of an algebraic type
(called |Dp a| in Section~\ref{sec:qdsl-vs-edsl}), with constructs
that represent |while| and manifest arrays similar to those
above. A backend then compiles values of type |Dp a| to C code.
QDSL~Feldspar provides a transformer from |Qt a| to |Dp a|, and
shares the EDSL~Feldspar backend.

The transformer from |Qt| to |Dp| performs the following steps.
\begin{itemize}
\item In any context where a constant $c$ is not fully applied,
  it replaces $c$ with $\expabs{\overline{x}}{}{c \app \overline{x}}$.
  It replaces identifiers connected to the type |Maybe|, such as
  |return|, |(>>=)|, and |maybe|, by their definitions.
\item It normalises the term to ensure the subformula property,
  using the rules of Section~\ref{sec:subformula}.
  The normaliser does not support all Haskell data types,
  but does support tuples, and the types |Maybe| and |Vec|.
% \item Overloading \\
%   \todo{Say something about how overloading for arithmetic is handled.}
\item It traverses the term, converting |Qt| to |Dp|.
  It checks that only permitted primitives appear in |Qt|,
  and translates these to their corresponding representation
  in |Dp|. Permitted primitives include:
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
translator from |Qt a| to |Dp a| is forced to re-infer all the type
information for the subterms of the term of type |Qt a|.  This is why
we translate the |Maybe| monad as a special case, rather than
supporting overloading for monad operations.

%%  As we noted in the introduction, rather than build a special-purpose tool for
%%  each QDSL, it should be possible to design a single tool for each host language.
%%  In the conclusion, we sketch the design of a general purpose tool for Haskell QDSL,
%%  including support for type inference and overloading.

We measured the behaviour of five benchmark programs.
\begin{center}
\begin{tabular}{l@@{\quad}l}
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
Runs for EDSL are shown both with and without common subexpression elimination (CSE),
which is supported by a simple form of observable sharing. QDSL does not require CSE,
since the normalisation algorithm preserves sharing. One benchmark, FFT, exhausts
memory without CSE. All benchmarks produce essentially the same C for both QDSL
and EDSL, which run in essentially the same time. The one exception is FFT, where
Feldspar appears to introduce spurious conversions that increase the
runtime.

Measurements were done on a quad-core Intel i7-2640M CPU
running at 2.80 GHz and 3.7 GiB of RAM, with GHC Version 7.8.3 and
GCC version 4.8.2, running on Ubuntu 14.04 (64-bit).


\section{The subformula property}
\label{sec:subformula}

This section introduces a collection of reduction rules for
normalising terms that enforces the subformula property
while ensuring sharing is preserved. The rules adapt to
both call-by-need and call-by-value.

We work with simple types. The only polymorphism in our examples
corresponds to instantiating constants (such as $\mathit{while}$) at
different types.

Types, terms, and values are presented in Figure~\ref{fig:term}.
Let $A$, $B$, $C$ range over types, including base types ($\iota$),
functions ($A \to B$), products ($A \times B$), and sums ($A + B$).
Let $L$, $M$, $N$ range over terms, and $x$, $y$, $z$ range over
variables.  Let $c$ range over constants, which are fully
applied according to their arity, as discussed below.
As usual, terms are taken as
equivalent up to renaming of bound variables. Write $\fv{M}$ for
the set of free variables of $M$, and $\subst{N}{x}{M}$ for
capture-avoiding substitution of $M$ for $x$ in $N$.
%
%% [SL: not relevant]
%% Types correspond to propositions and terms to proofs in
%% minimal propositional logic.
%
Let $V$, $W$ range over values, and $\nv$ range over
terms that are not values.

Let $\Gamma$ range over type environments, which are sets of pairs
of variables with types $x:A$. Write $\Gamma \vdash M:A$ to
indicate that term $M$ has type $A$ under type environment
$\Gamma$. Typing rules are standard.

% The grammar of normal forms is given in Figure~\ref{fig:nf}. We reuse
% $L,M,N$ to range over terms in normal form and $V,W$ to range over
% values in normal form, and we let $Q$ range over neutral forms.

Reduction rules for normalisation are presented in Figure~\ref{fig:norm}.
The rules are confluent, so order of application is irrelevant to the
final answer, but we break them into three phases to ease the proof
of strong normalisation. It is easy to confirm that all
of the reduction rules preserve sharing and preserve order of evaluation.

Write $M \mapsto_i N$ to indicate that $M$ reduces to $N$ in phase
$i$. Only Phases~1 and 2 are required to normalise terms, but if the
semantics is call-by-need then Phase~3 may also be applied.
Let $F$ and $G$ range over two different forms of evaluation
frame used in Phases~1 and~2 respectively. We write $\fv{F}$ for the
set of free variables of $F$, and similarly for $G$.  The reduction
relation is closed under compatible closure.

\figterm
%\figtyping
% \fignf
\fignorm

The normalisation procedure consists of exhaustively applying the
reductions of Phase~1 until no more apply, then similarly for Phase~2,
and finally for Phase~3.  Phase~1 performs let-insertion, naming
subterms that are not values, along the lines of a translation to
A-normal form \citep{a-normal-form} or reductions (let.1) and (let.2)
in Moggi's metalanguage for monads \citep{Moggi-1991}.  Phase~2
performs two kinds of reduction: $\beta$ rules apply when an
introduction (construction) is immediately followed by an elimination
(deconstruction), and $\kappa$ rules push eliminators closer to
introducers to enable $\beta$ rules.  Phase~3 ``garbage collects''
unused terms as in the call-by-need lambda calculus
\citep{call-by-need}. Phase~3 should be omitted if the intended
semantics of the target language is call-by-value rather than
call-by-need.

Every term has a normal form.
\begin{proposition}[Strong normalisation]
Each of the reduction relations $\rewrite{i}$ is confluent and strongly
normalising: all $\rewrite{i}$ reduction sequences on well-typed
terms are finite.
\end{proposition}
The only non-trivial proof is for $\rewrite{2}$, which can be proved
via a standard reducibility argument (see, for example,
\cite{Lindley07}). If the target language includes general recursion,
normalisation should treat the fixpoint operator as an uninterpreted
constant.

% The grammar of Figure~\ref{fig:nf} characterises normal forms
% precisely.
% \begin{proposition}[Normal form syntax]
% \label{prop_normal}
% An expression $N$ matches the syntax of normal forms in
% Figure~\ref{fig:nf} if and only if it is in normal form with regard to
% the reduction rules of Figure~\ref{fig:norm}.
% \end{proposition}

The \emph{subformulas} of a type are the type itself and its
components. For instance, the subformulas of $A \to B$ are itself and
the subformulas of $A$ and $B$. The \emph{proper subformulas} of a
type are all its subformulas other than the type itself.

The \emph{subterms} of term are the term itself and its components.
For instance, the subterms of $\expabs{x}{}{N}$ are itself and the
subterms of $N$ and the subterms of $\expapp{L}{M}$ are itself and the
subterms of $L$ and $M$. The \emph{proper subterms} of a
term are all its subterms other than the term itself.

Constants are always fully applied; they are introduced as a
separate construct to avoid consideration of irrelevant subformulas
and subterms.
The type of a constant $c$ of arity $k$ is written
\[
c : A_1 \to \cdots A_k \to B
\]
and its subtypes are itself and $A_1$, \ldots, $A_k$, and $B$
(but not $A_i \to \ldots \to A_k \to B$ for $i > 1$).
An application of a constant $c$ of arity $k$ is written
\[
c \app M_1 \app \cdots \app M_k
\]
and its subterms are itself and $M_1$, \ldots, $M_k$
(but not $c \app M_1 \app \cdots \app M_j$ for $j < k$).
Free variables are equivalent to constants of arity zero.

Terms in normal form satisfy the subformula property.

\begin{proposition}[Subformula property]
\label{prop_subformula}
If $\Gamma \vdash M:A$ and $M$ is in normal form,
then every subterm of $M$ has a type that is either a subformula of $A$,
a subformula of a type in $\Gamma$, or a subformula of the type
of a constant in $M$.
\end{proposition}
The proof follows the lines of \citet{Prawitz-1965}.
The differences are that we have introduced fully applied constants
(to enable the sharpened subformula property, below), and that our
reduction rules introduce |let|, in order to ensure sharing is preserved.

Normalisation may lead to an exponential increase in the size
of a term, for instance when there are nested |case| expressions. This
was not a problem for the examples we considered in
Section~\ref{sec:implementation}, but may be a problem in some
contexts. Normalisation may be controlled by introduction of
uninterpreted constants (see Section~\ref{subsec:subformula}),
but further work is needed to understand the contexts in which
complete normalisation is
desirable and the contexts in which it is problematic.

Examination of the proof in \citet{Prawitz-1965} shows that in fact
normalisation achieves a sharper property.
\begin{proposition}[Sharpened subformula property]
If $\Gamma \vdash M:A$ and $M$ is in normal form, then every proper
subterm of $M$ that is not a free variable or a subterm of a constant
application has a type that is a proper subformula of $A$ or a proper
subformula of a type in $\Gamma$.
\end{proposition}

The sharpened subformula property says nothing about the type of
subterms of constant applications, but this is immediately apparent by
recursive application of the sharpended subformula property.  Given a
subterm that is a constant application $c \app \overline{M}$, where
$c$ has type $\overline{A} \to B$, then the subterm itself has type
$B$, each subterm $M_i$ has type $A_i$, and every proper subterm of
$M_i$ that is not a free variable of $M_i$ or a subterm of a constant
application has a type that is a proper subformula of $A_i$ or a
proper subformula of the type of one of its free variables.

In Section~\ref{sec:qfeldspar}, an important property is that every
top-level term passed to |qdsl| is suitable for translation to C after
normalisation.  Here we are interested in C as a \emph{first-order}
language. The exact property required is somewhat subtle. One might at
first guess the required property is that every subterm is
representable, in the sense introduced in
Section~\ref{subsec:top}, but this is not quite right. The
top-level term is a function from a representable type to a
representable type. And the constant |while| expects subterms of type
|s -> Bool| and |s -> s|, where the state |s| is representable.
Fortunately, the exact property required is not hard to formulate in a
general way, and is easy to ensure by applying the sharpened
subformula property.

We introduce a variant of the usual notion of \emph{rank} of a type,
with respect to a notion of representability.  A term of type |A -> B|
has rank $\min(m+1,n)$ where $m$ is the rank of |A| and $n$ is the
rank of |B|, while a term of representable type has rank $0$.

The property we need to ensure ease of translation to C
(or any other first-order language) is as follows.

\begin{proposition}[Rank and representability]
\label{prop:rank}
Consider a term $M$ of rank $1$, where every free variable of $M$ has
rank $0$ and every constant in $M$ has rank at most $2$.  Then $M$
normalises to a form where every subterm either is of representable
type or is of the form $\expabs{\overline{x}}{}{N}$ where each of the
bound variables $x_i$ and the body $N$ has representable type.
\end{proposition}

The property follows immediately by observing that any term $L$
with type of rank $1$ can be rewritten in the form
$\expabs{\overline{x}}{}{L \app \overline{x}}$ where each bound variable
and the body has representable type, and then normalising and applying
the sharpened subformula property.


\section{Feldspar as an EDSL}
\label{sec:qdsl-vs-edsl}

This section reviews the combination of deep and shallow embeddings
required to implement Feldspar as an EDSL, and considers the trade-offs
between the QDSL and EDSL approaches.
Much of this section reprises \citet{svenningsson:combining}.

The top-level function of EFeldspar has the type:
\begin{spec}
qdsl :: (Rep a , Rep b) => (Dp a -> Dp b) -> C
\end{spec}
Here |Dp a| is the deep representation of a term of type |a|.
The deep representation is described in detail in Section~\ref{subsec:deep}
below, and is chosen to be easy to translate to C.
As before, type |C| represents code in C,
and type class |Rep| restricts to representable types.


\subsection{A first example}
\label{subsec:e-power}

Here is the power function of Section~\ref{subsec:power},
now represented using EDSL:
\begin{code}
power :: Int -> Dp Float -> Dp Float
power n x  =
  if n < 0 then
    x .==. 0 ? (0,  1 / power (-n) x)
  else if n == 0 then
    1
  else if even n then
    sqr (power (n `div` 2) x)
  else
    x * power (n-1) x

sqr :: Dp Float -> Dp Float
sqr y = y * y
\end{code}
Type |Q (Float -> Float)| in the QDSL variant becomes |Dp Float -> Dp Float|
in the EDSL variant, meaning that |power n| accepts a representation
of the argument and returns a representation of that argument raised
to the $n$'th power.

In EDSL, no quotation is required, and the code looks almost---but not quite!---like
an unstaged version of power, but with different types.  Clever encoding tricks,
explained later, permit declarations, function calls, arithmetic
operations, and numbers to appear the same whether they are to be
executed at generation-time or run-time.  However,
as explained later, comparison and conditionals appear differently
depending on whether they are to be executed at generation-time or
run-time, using |M == N| and |if L then M else N| for the former but
|M .==. N| and |L ?  (M, N)| for the latter.

Evaluating |power (-6)| yields the following:
\begin{spec}
(\ u -> (u .==. 0) ? (0,
  1 / (  (u * ((u * 1) * (u * 1))) *
         (u * ((u * 1) * (u * 1))))))
\end{spec}
Applying common-subexpression elimination, or using a technique such
as observable sharing, permits recovering the sharing structure.
\[
\begin{array}{c@@{~~}||@@{~~}l}
|v| & |(u * 1)|  \\
|w| & |u * (v * v)| \\
\text{top} & |(u .==. 0) ? (0, 1/(w*w))|
\end{array}
\]
From the above, it is easy to generate the final C code,
which is identical to that in Section~\ref{subsec:power}.

Here are points of comparison between the two approaches.
\begin{itemize}

\item A function |a -> b| is embedded in
QDSL as |Qt (a -> b)|, a representation of a function, and in
EDSL as |Dp a -> Dp b|, a function between representations.

\item QDSL enables the host and embedded languages to appear
identical.  In contrast, EDSL requires some term forms, such as
comparison and conditionals, to differ between the host and embedded
languages.

\item QDSL requires syntax to separate quoted and unquoted terms. In
contrast, EDSL permits the host and embedded languages to intermingle
seamlessly. Depending on you point of view, explicit quotation syntax
may be considered as an unnessary distraction or as drawing a useful
distinction between generation-time and run-time.  If one takes the
former view, the type-based approach to quotation found in C\# and
Scala might be preferred.

\item QDSL may share the same representation for quoted terms across a
range of applications; the quoted language is the host language, and
does not vary with the specific domain.  In contrast, EDSL typically
develops custom shallow and deep embeddings for each application,
although these may follow a fairly standard pattern.

\item QDSL yields an unwieldy term that requires normalisation.  In
contrast, EDSL yields the term in normalised form in this case, though
there are other situations where a normaliser is required (see
Section~\ref{subsec:e-maybe}).

\item QDSL requires traversing the quoted term to ensure it only
mentions permitted identifiers. In contrast, EDSL guarantees that if a
term has the right type it will translate to the target.  If the
requirement to eyeball code to ensure only permitted identifiers are
used is considered too onerous, it should be easy to build a
preprocessor that checks this property. In Haskell, it is easy to
incorporate such a preprocessor using MetaHaskell \cite{metahaskell}.

\item Since QDSLs may share the same quoted terms across a range of
applications, the cost of building a normaliser or a preprocessor
might be amortised.  In the conclusion, we consider the design of a
tool for building QDSLs that may perform both these functions.

\item QDSL preserves sharing. In contrast, EDSL loses sharing, which
must later be recovered either by common subexpression elimination or
by applying a technique such as observable sharing.

\item Once the deep embedding or the normalised quoted term is
produced, generating the domain-specific code is similar for both
approaches.

\end{itemize}


\subsection{A second example}
\label{subsec:e-maybe}

In Section~\ref{subsec:maybe}, we exploited the |Maybe| type to refactor the code.

In EDSL, we must use a new type, where
|Maybe|, |Nothing|, |Just|, and |maybe| become
|Opt|, |none|, |some|, and |option|,
and |return| and |(>>=)| are similar to before.
\begin{spec}
type Opt a
none   	::  Undef a => Opt a
some    ::  a -> Opt a
return 	::  a -> Opt a
(>>=)  	::  Opt a -> (a -> Opt b) -> Opt b
option 	::  (Undef a, Undef b) =>
              b -> (a -> b) -> Opt a -> b
\end{spec}
Type class |Undef| is explained in Section~\ref{subsec:undef},
and details of type |Opt| are given in Section~\ref{subsec:opt}.

% In order to be easily represented in C, type |Opt a| is represented as
% a pair consisting of a boolean and the representation of the type |a|.
% For |none|, the boolean is false and a
% default value of type |a| is provided.
% For |some|, the boolean is true.

Here is the refactored code.
\begin{code}
power' :: Int -> Dp Float -> Opt (Dp Float)
power' n x =
  if n < 0 then
    (x .==. 0) ? (  none,
                    do  y <- power' (-n) x
                        return (1 / y))
  else if n == 0 then
    return 1
  else if even n then
    do  y <- power' (n `div` 2) x
        return (sqr y)
  else
    do  y <- power' (n-1) x
        return (x*y)

power'' ::  Int -> Dp Float -> Dp Float
power'' n x  =  option 0 (\y -> y) (power' n x)
\end{code}
Here |sqr| is as before.

The term of type |Dp Float| generated by
evaluating |power (-6) x| is large and unscrutable:
\begin{spec}
(((fst ((x == 0.0) ? (((False ? (True, False)), (False ?
(undef, undef))), (True, (1.0 / ((x * ((x * 1.0) * (x *
1.0))) * (x * ((x * 1.0) * (x * 1.0))))))))) ? (True,
False)) ? (((fst ((x == 0.0) ?  (((False ? (True, False)),
(False ?  (undef, undef))), (True, (1.0 / ((x * ((x * 1.0) *
(x * 1.0))) * (x * ((x * 1.0) * (x * 1.0))))))))) ?  ((snd
((x == 0.0) ?  (((False ? (True, False)), (False ?  (undef,
undef))), (True, (1.0 / ((x * ((x * 1.0) * (x * 1.0))) *
(x * ((x * 1.0) * (x * 1.0))))))))), undef)), 0.0))
\end{spec}
Before, evaluating |power| yielded a term essentially in normal
form.  However, here rewrite rules need to be repeatedly applied,
similar to those required to enforce the subformula property
as described in Section~\ref{sec:subformula}.
After applying these rules, common subexpression
elimination yields the same structure as in the previous subsection,
from which the same C code is generated.

% Rewrite rules including the following need to be
% repeatedly applied.
% \begin{eqnarray*}
% |fst (M, N)| &\leadsto& M \\
% |snd (M, N)| &\leadsto& N \\
% |fst (L ? (M, N))| &\leadsto& |L ? (fst M, fst N)| \\
% |snd (L ? (M, N))| &\leadsto& |L ? (snd M, snd N)| \\
% |True ? (M,N)| &\leadsto& |M| \\
% |False ? (M,N)| &\leadsto& |N| \\
% |(L ? (M, N)) ? (P,Q)| &\leadsto& |L ? (M ? (P,Q)) ? (N ? (P,Q))| \\
% |L ? (M, N)| &\leadsto& |L ? (M^[L:=True], N^[L:=False])|
% \end{eqnarray*}
% Here |L|,|M|,|N|,|P|,|Q| range over |Dp| terms, and
% |M[L:=P]| stands for |M| with each occurence of |L| replaced by |P|.

Hence, an advantages of the EDSL approach---that it generates
terms essentially in normal form---turns out to be restricted
to a limited set of types, including functions and products,
but excluding sums. If one wishes to deal with sum types,
separate normalisation is required. This is one reason
why we do not consider normalisation as required by QDSL
to be particularly onerous.

Here are points of comparison between the two approaches.
\begin{itemize}

\item Both QDSL and EDSL can exploit notational conveniences in the
host language. The example here exploits Haskell |do| notation; the
embedding of SQL in F\# by \citet{cheney:linq} expoits F\# sequence
notation. For EDSL, exploiting |do| notation just requires
instantiating |return| and |(>>=)| correctly. For QDSL, it is
also necessary for the normaliser to recognise and expand
|do| notation and to substitute appropriate instances of
|return| and |(>>=)|.

\item As this example shows, sometimes both QDSL and EDSL may require
normalisation.  As mentioned previously, for QDSLs the cost of
building a normaliser might be amortised across several applications.
In constrast, each EDSL usually has a distinct deep representation and
so requires a distinct normaliser.

\end{itemize}


\subsection{The deep embedding}
\label{subsec:deep}

Recall that a value of type |Dp a| represents a term of type |a|,
and is called a deep embedding.
\begin{code}
data Dp a where
  LitB	    ::  Bool -> Dp Bool
  LitI	    ::  Int -> Dp Int
  LitF	    ::  Float -> Dp Float
  If	    ::  Dp Bool -> Dp a -> Dp a -> Dp a
  While	    ::  (Dp a -> Dp Bool) ->
                  (Dp a -> Dp a) -> Dp a -> Dp a
  Pair	    ::  Dp a -> Dp b -> Dp (a,b)
  Fst	    ::  Dp (a,b) -> Dp a
  Snd       ::  Dp (a,b) -> Dp b
  Prim1	    ::  String -> Dp a -> Dp b
  Prim2     ::  String -> Dp a -> Dp b -> Dp c
  Arr       ::  Dp Int -> (Dp Int -> Dp a) -> Dp (Arr a)
  ArrLen    ::  Dp (Arr a) -> Dp Int
  ArrIx     ::  Dp (Arr a) -> Dp Int -> Dp a
  Variable  ::  String -> Dp a
\end{code}
The type above represents a low level, pure functional language
with a straightforward translation to C. It uses higher-order
abstract syntax (HOAS) to represent constructs with variable binding
\citet{hoas}.

The deep embedding has boolean, integer, and floating point literals,
conditionals, while loops, pairs, primitives, arrays,
and special-purpose constructs for variables and values.
Constructs |LitB|, |LitI|, |LitF| build literals.
Construct |If| builds a conditional.
Construct |While| resembles |while| in Section~\ref{subsec:while}.
Constructs |Pair|, |Fst|, and |Snd| build pairs and
extract the first and second component.
Constructs |Prim1| and |Prim2| represent primitive
operations, the string is the name of the operation.
Construct |ArrIx| creates a new array from a length and
a body that computes the array element
for each index, construct |ArrLen| extracts the length from an array, and
construct |ArrIx| fetches the element at a given index.
Construct |Variable| represents a variable.

% The exact semantics is given by the evaluator, |eval|,
% shown in Figure~\ref{fig:eval}. It is a strict language,
% so we define an infix strict application operator |(<*>)|.
% Function |eval| plays no role in generating C, but may be useful for testing.
%
% \begin{figure*}
% \begin{code}
% (<*>)                 ::  (a -> b) -> a -> b
% f <*> x               =   seq x (f x)
%
% eval                  ::  Dp a -> a
% eval (LitI i)         =   i
% eval (LitF x)         =   x
% eval (LitB b)         =   b
% eval (If c t e)       =   if eval c then eval t else eval e
% eval (While c b i)    =   evalWhile (evalFun c) (evalFun b) <*> eval i
% eval (Pair a b)       =   (,) <*> eval a <*> eval b
% eval (Fst p)          =   fst <*> eval p
% eval (Snd p)          =   snd <*> eval p
% eval (Prim1 _ f a)    =   f <*> eval a
% eval (Prim2 _ f a b)  =   f <*> eval a <*> eval b
% eval (Arr n g)        =   array (0,n') [ (i, eval (g (LitI i))) | i <- [0..n'] ]
%                           where n' = eval n - 1
% eval (ArrLen a)       =   u-l+1  where (l,u) = bounds (eval a)
% eval (ArrIx a i)      =   eval a ! eval i
% eval (Value v)        =   v
%
% evalFun               ::  (Dp a -> Dp b) -> a -> b
% evalFun f x           =   (eval . f . Value) <*> x
%
% evalWhile             ::  (a -> Bool) -> (a -> a) -> a -> a
% evalWhile c b i       =   if c i then evalWhile c b <*> b i else i
% \end{code}
% \caption{Evaluator for the deep embedding}
% \label{fig:eval}
% \end{figure*}

\subsection{Class |Syn|}

We introduce a type class |Syn| that allows us to convert
shallow embeddings to and from deep embeddings.
\begin{code}
class Syn a where
  type Internal a
  toDp    ::  a -> Dp (Internal a)
  fromDp  ::  Dp (Internal a) -> a
\end{code}
Type |Internal| is a GHC type family \citep{type-families}.  Functions
|toDp| and |fromDp| translate between the shallow embedding |a| and the
deep embedding |Dp (Internal a)|.

The first instance of |Syn| is |Dp| itself, and is straightforward.
\begin{code}
instance Syn (Dp a) where
  type Internal (Dp a) = a
  toDp    =  id
  fromDp  =  id
\end{code}
Our representation of a run-time |Bool| will have type |Dp Bool| in
both the deep and shallow embeddings, and similarly for |Int| and
|Float|.

We do not code the target language using its constructs
directly. Instead, for each constructor we define a corresponding
``smart constructor'' using class |Syn|.
\begin{code}
true, false :: Dp Bool
true = LitB True
false = LitB False

(?) :: Syn a => Dp Bool -> (a,a) -> a
c ? (t,e) = fromDp (If c (toDp t) (toDp e))

while :: Syn a => (a -> Dp Bool) -> (a -> a) -> a -> a
while c b i = fromDp (While  (c . fromDp)
                             (toDp . b . fromDp)
                             (toDp i))
\end{code}

Numbers are made convenient to manipulate via overloading.
\begin{code}
instance Num (Dp Int) where
  a + b  =  Prim2 "(+)" a b
  a - b  =  Prim2 "(-)" a b
  a * b  =  Prim2 "(*)" a b
  fromInteger a = LitI (fromInteger a)
\end{code}
With this declaration, |1+2 :: Dp Int| evaluates to
\[
|Prim2 "(+)" (LitI 1) (LitI 2)|,
\]
permitting code executed at generation-time and run-time to
appear identical.  A similar declaration works for |Float|.

Comparison also benefits from smart constructors.
\begin{code}
(.==.) :: (Syn a, Eq (Internal a)) => a -> a -> Dp Bool
a .==. b = Prim2 "(==)" (toDp a) (toDp b)

(.<.) :: (Syn a, Ord (Internal a)) => a -> a -> Dp Bool
a .<. b = Prim2 "(<)" (toDp a) (toDp b)
\end{code}
Overloading cannot apply here, because Haskell requires
|(==)| return a result of type |Bool|, while |(.==.)| returns
a result of type |Dp Bool|, and similarly for |(.<.)|.


\subsection{Embedding pairs}

We set up a correspondence between host language pairs
in the shallow embedding and target language pairs in the deep embedding.
\begin{code}
instance (Syn a, Syn b) => Syn (a,b) where
  type  Internal (a,b) = (Internal a, Internal b)
  toDp (a,b)  =  Pair (toDp a) (toDp b)
  fromDp p    =  (fromDp (Fst p), fromDp (Snd p))
\end{code}
This permits us to manipulate pairs as normal, with |(a,b)|, |fst a|,
and |snd a|.  Argument |p| is duplicated in the definition of
|fromDp|, which may require common subexpression elimination
as discussed in Section~\ref{subsec:e-power}.

We have now developed sufficient machinery to define a |for| loop
in terms of a |while| loop.
\begin{code}
for :: Syn a => Dp Int -> a -> (Dp Int -> a -> a) -> a
for n s_0 b = snd (while  (\(i,s) -> i .<. n)
                          (\(i,s) -> (i+1, b i s))
                          (0,s_0))
\end{code}
The state of the |while| loop is a pair consisting of a counter and
the state of the |for| loop. The body |b| of the |for| loop is a function
that expects both the counter and the state of the |for| loop.
The counter is discarded when the loop is complete, and the final state
of the |for| loop returned.

Thanks to our machinery, the above definition uses only ordinary Haskell
pairs. The condition and body of the |while| loop pattern match on the
state using ordinary pair syntax, and the initial state is constructed
as an ordinary pair.


\subsection{Embedding undefined}
\label{subsec:undef}

For the next section, which defines an analogue of the |Maybe| type, it
will prove convenient to work with types which have a distinguished
value at each type, which we call |undef|.

% (A better name might be `default' or `undefined', if each did not
% already have another meaning in Haskell.)

It is straightforward to define a type class |Undef|, where type |a|
belongs to |Undef| if it belongs to |Syn| and it has an
undefined value.
\begin{code}
class Syn a => Undef a where
  undef :: a

instance Undef (Dp Bool) where
  undef = false

instance Undef (Dp Int) where
  undef = 0

instance Undef (Dp Float) where
  undef = 0

instance (Undef a, Undef b) => Undef (a,b) where
  undef = (undef, undef)
\end{code}

For example,
\begin{code}
(/#)    ::  Dp Float -> Dp Float -> Dp Float
x /# y  =   (y .==. 0) ? (undef, x/y)
\end{code}
behaves as division, save that when the divisor is zero
it returns the undefined value of type |Float|, which
is also zero.

\citet{svenningsson:combining} claim that it is not possible to support
|undef| without changing the deep embedding, but here we have defined |undef|
entirely as a shallow embedding.  (It appears they underestimated the
power of their own technique!)


\subsection{Embedding option}
\label{subsec:opt}

We now explain in detail the |Opt| type seen in Section~\ref{subsec:maybe}.

The deep-and-shallow technique cleverly represents deep embeddding
|Dp (a,b)| by shallow embedding |(Dp a, Dp b)|.  Hence, it is tempting to
represent |Dp (Maybe a)| by |Maybe (Dp a)|, but this cannot work,
because |fromDp| would have to decide at generation-time whether to
return |Just| or |Nothing|, but which to use is not known until
run-time.

Instead, \citet{svenningsson:combining} represent values of type
|Maybe a| by the type |Opt_R a|, which pairs a boolean with a value of
type |a|.  For a value corresponding to |Just x|, the boolean is true
and the value is |x|, while for one corresponding to |Nothing|, the
boolean is false and the value is |undef|.  We define |some_R|,
|none_R|, and |opt_R| as the analogues of |Just|, |Nothing|, and
|maybe|.  The |Syn| instance is straightforward, mapping options to
and from the pairs already defined for |Dp|.
\begin{code}
data Opt_R a = Opt_R { def :: Dp Bool, val :: a }

instance Syn a => Syn (Opt_R a) where
  type Internal (Opt_R a) = (Bool, Internal a)
  toDp (Opt_R b x)  =  Pair b (toDp x)
  fromDp p          =  Opt_R (Fst p) (fromDp (Snd p))

some_R          ::  a -> Opt_R a
some_R x        =   Opt_R true x

none_R          ::  Undef a => Opt_R a
none_R          =   Opt_R false undef

option_R        ::  Syn b => b -> (a -> b) -> Opt_R a -> b
option_R d f o  =   def o ? (f (val o), d)
\end{code}

The next obvious step is to define a suitable monad over the type |Opt_R|.
The natural definitions to use are as follows:
\begin{code}
return    ::  a -> Opt_R a
return x  =   some_R x

(>>=)     ::  (Undef b) => Opt_R a -> (a -> Opt_R b) -> Opt_R b
o >>= g   =   Opt_R  (def o ? (def (g (val o)), false))
                     (def o ? (val (g (val o)), undef))
\end{code}
However, this adds type constraint |Undef b|
to the type of |(>>=)|, which is not permitted.
The need to add such constraints often arises, and has
been dubbed the constrained-monad problem
\citep{hughes1999restricted,SculthorpeBGG13,SvenningssonS13}.
We solve it with a trick due to \citet{PerssonAS11}.

We introduce a second continuation-passing style (cps) type |Opt|,
defined in terms of the representation type |Opt_R|.  It is
straightforward to define |Monad| and |Syn| instances for the cps
type, operations to lift the representation type to cps and to lower
cps to the representation type, and to lift |some|, |none|, and
|option| from the representation type to the cps type.
The |lift| operation is closely related to the |(>>=)| operation
we could not define above; it is properly typed,
thanks to the type constraint on |b| in the definition of |Opt a|.

\begin{code}
newtype Opt a =
  O { unO :: forall b . Undef b => ((a -> Opt_R b) -> Opt_R b) }

instance Monad Opt where
  return x    =  O (\g -> g x)
  m >>= k     =  O (\g -> unO m (\x -> unO (k x) g))

instance Undef a => Syn (Opt a) where
  type Internal (Opt a) = (Bool, Internal a)
  fromDp  =  lift . fromDp
  toDp    =  toDp . lower

lift     ::  Opt_R a -> Opt a
lift o   =   O (\g -> Opt_R  (def o ? (def (g (val o)), false))
                             (def o ? (val (g (val o)), undef)))

lower    ::  Undef a => Opt a -> Opt_R a
lower m  =   unO m some_R

none     ::  Undef a => Opt a
none     =   lift none_R

some     ::  a -> Opt a
some a   =   lift (some_R a)

option        ::  (Undef a, Undef b) =>
                    b -> (a -> b) -> Opt a -> b
option d f o  = option_R d f (lower o)
\end{code}
These definitions support the EDSL code presented
in Section~\ref{subsec:e-maybe}.


\subsection{Embedding vector}

Recall that values of type |Array| are created by construct |Arr|,
while |ArrLen| extracts the length and |ArrIx| fetches the element at
the given index.  Corresponding to the deep embedding |Array| is a
shallow embedding |Vec|.
\begin{code}
data Vec a = Vec (Dp Int) (Dp Int -> a)

instance Syn a => Syn (Vec a) where
  type Internal (Vec a) = Array Int (Internal a)
  toDp (Vec n g)    =  Arr n (toDp . g)
  fromDp a          =  Vec  (ArrLen a)
                          (\i -> fromDp (ArrIx a i))

instance Functor Vec where
  fmap f (Vec n g)  =  Vec n (f . g)
\end{code}
The constructor |Vec| resembles the constructor |Arr|, but the former
constructs a high-level representation of the array and the latter an
actual array.
It is straightforward to make |Vec| an instance of |Functor|.

It is straightforward to define operations on vectors,
including combining corresponding elements of two vectors,
summing the elements of a vector, dot product of two vectors,
and norm of a vector.
\begin{code}
zipVec  ::  (Syn a, Syn b) =>
              (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipVec f (Vec m g) (Vec n h)
        =   Vec (m `min` n) (\i -> f (g i) (h i))

sumVec  ::  (Syn a, Num a) => Vec a -> a
sumVec (Vec n g)
        =   for n 0 (\i x -> x + g i)

dotVec      ::  (Syn a, Num a) => Vec a -> Vec a -> a
dotVec u v  =   sumVec (zipVec (*) u v)

normVec     ::  Vec Float -> Dp Float
normVec v   =   sqrt (dotVec v v)
\end{code}

The vector representation makes it easy to define any functions when
each vector element is computed independently, including |drop|,
|take|, |reverse|, vector concatentation, and the like, but is less
well suited to functions with dependencies between elements, such as
computing a running sum.

An important consequence of the style of definition we have adopted is
that it provides lightweight fusion. The definition of |dotVec| would not produce
good C code if it first computed |zipVec (*) u v|, put the result into an
intermediate vector |w|, and then computed |sumVec w|. Fortunately, it does
not. Assume |u| is |Vec m g| and |v| is |Vec n h|. Then we can simplify
|dotVec u v| as follows:
\[
\begin{array}{cl}
         &  |toVec u v|  \\
\leadsto &  |sumVec (zipVec (*) u v)|  \\
\leadsto &  |sumVec (zipVec (*) (Vec m g) (Vec n h)|  \\
\leadsto &  |sumVec (Vec (m `min` n) (\i -> g i * h i)|  \\
\leadsto &  |for (m `min` n) (\i x -> x + g i * h i)|
\end{array}
\]
Indeed, we can see that by construction that whenever we combine two
primitives the intermediate vector is always eliminated, a stronger
guarantee than provided by conventional optimising compilers.

The type class |Syn| enables conversion between types |Arr| and |Vec|.
Hence for EDSL, unlike QDSL, explicit calls |toVec| and |fromVec| are
not required.  Invoking |edsl normVec| produces the same C code as in
Section~\ref{subsec:arrays}.

As with QDSL, there are some situations where fusion is not beneficial.
We may materialise a vector as an array with the following function.
\begin{code}
memorise :: Syn a => Vec a -> Vec a
memorise (Vec n g) =
  = Vec n (\i -> fromDp (ArrIx (Arr n (toDp . g)) i))
\end{code}
The above definition depends on common subexpression elimination
to ensure |Arr n (toDp . g)| is computed once, rather than once
for each element of the resulting vector.
If
\begin{spec}
blur :: Syn a => Vec a -> Vec a
\end{spec}
averages adjacent elements of a vector, then one may choose to
compute either
\begin{center}
|blur . blur| ~~~or~~~ |blur . memorise . blur|
\end{center}
with different trade-offs between recomputation and memory usage.

EDSL silently converts between representation, while QDSL forces
all conversions to be written out; following the pattern that EDSL
is more compact, while QDSL is more explicit.  For QDSL it is the
subformula property which guarantees that all intermediate uses of
|Vec| are eliminated, while for EDSL this is established by
operational reasoning on the behaviour of the type |Vec|.


\section{Related work}
\label{sec:related}

DSLs have a long and rich history \citep{Bentley-1986}.
An early use of quotation in programming is Lisp \citep{McCarthy-1960},
and perhaps the first application of quotation to domain-specific
languages is Lisp macros \citep{Hart-1963}.

This paper uses Haskell, which has been widely used for EDSLs
\citet{hudak1997domain, reid1999prototyping, bjesse1998lava,
Gill:14:DSLs-and-Synthesis}.  We constrast QDSL with an EDSL technique
that combines deep and shallow embedding, as described by
\citet{svenningsson:combining}, and as used in several Haskell EDSLs
including Feldspar \citep{FELDSPAR}, Obsidian
\citep{svensson2011obsidian}, Nikola \citep{NIKOLA}, Hydra
\citep{giorgidze2011embedding}, and Meta-Repa \citep{ankner2013edsl}.

% The vector type used in this paper is one of several types which
% enjoy fusion. Other examples include push
% arrays \citep{claessen2012expressive} and sequential arrays
% and streams as used in Feldspar \citep{feldspar-github}.

\citet{odonnell:sharing} identified loss of sharing in the context of
embedded circuit descriptions.  \citet{claessen1999observable} and
\citet{gill2009type} propose solutions to this problem via extensions
to Haskell that permit observable sharing.

A proposition-as-types principle for quotation as a modal logic was
proposed by \citet{Davies-Pfenning-1996,Davies-Pfenning-2001}.  As
they note in that paper, their technique has close connections to
two-level languages \citep{Nielson-2005}.

Other approaches to DSL that make use of quotation include
C\# and F\# versions of LINQ under .NET
\citep{csharplinq,fsharplinq} and Scala Lightweight
Modular Staging \citep{scalalms}.
The underlying idea for QDSLs was established
for F\# LINQ by \citet{cheney:linq}.

\section{Conclusion}
\label{sec:conclusion}

\begin{quote}
A good idea can be much better than a new one. \\
\flushr -- Gerard Berry
\end{quote}

We have compared EDSLs and QDSLs, arguing that QDSLs offer competing
expressiveness and efficiency. EDSLs often (but not always) mimic the
syntax of the host language, and often (but not always) perform
normalisation in the host languages, while QDSLs (always) steal the
syntax of the host language, and (always) ensure the subformula property,
at the cost of requiring a normaliser, one per host language.

The subformula property may have applications in DSLs other that
QDSLs. For instance, after Section~\ref{subsec:opt} of this paper was
drafted, it occurred to us that a different approach to options in
EDSL would be to extend type |Dp| with constructs for type |Maybe|.
So long as type |Maybe| does not appear in the input or output of the
program, a normaliser that ensures the subformula property could
guarantee that C code for such constructs need never be generated.

As we noted in the introduction, rather than build a special-purpose tool for
each QDSL, it should be possible to design a single tool for each host language.
Our next step is to design Haskell QDSL, with the following features.
\begin{itemize}
\item Type inference for the terms returned from typed
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
|Qt| to |Dp| described at the beginning of Section~\ref{sec:implementation},
and lift most of its restrictions. For instance,
the current prototype is restricted to the |Maybe| monad, while the
envisioned tool will work with any monad.

Moli\`{e}re's Monsieur Jourdain was bemused to discover he had been
speaking prose his whole life. Similarly, many of us have used QDSLs for
years, if not by that name. DSL via quotation is the heart of Lisp
macros, Microsoft LINQ, and Scala LMS, to name but three. We hope that
by naming the concept and drawing attention to the central benefits of
normalisation and the subformula propety, we may help the concept to
flower further for years to come.

\paragraph*{Acknowledgement}
Najd is a recipient of the Google Europe Fellowship in Programming
Technology, and this research is supported in part by this Google
Fellowship. %% <-- Google's requested format
Svenningsson is a SICSA Visiting Fellow and is funded by a HiPEAC
collaboration grant and by the Swedish Foundation for Strategic
Research, under grant RawFP.
Lindley and Wadler were funded by EPSRC Grant EP/K034413/1.

\bibliographystyle{plainnat}
\bibliography{paper}


\end{document}
