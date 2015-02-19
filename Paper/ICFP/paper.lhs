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

%if False
\begin{code}

\end{code}
%endif

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
%include Section2.lhs

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
%include Section5.lhs

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
