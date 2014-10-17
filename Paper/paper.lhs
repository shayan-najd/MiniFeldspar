\documentclass{llncs}

%include polycode.fmt
%include forall.fmt

%format == = "\longeq "
%format || = "||"
%format `div` = "\rmdiv"
%format <*> = "\mathbin{{<}\!{*}\!{>}}"
%format .==. = "\mathbin{{.}{" == "}{.}}"
%format .<.  = "\mathbin{{.}{" < "}{.}}"
%format x_0
%format Opt_R
%format some_R
%format none_R
%format opt_R
%format power_Dp
%format power_Dp'
%format power_Dp''
%format sqr_Dp
%format power_Qt
%format power_Qt'
%format power_Qt''
%format sqr_Qt

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

\begin{document}

%if False
\begin{code}

\end{code}
%endif

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

\institute{The University of Edinburgh\\
\email{sh.najd@@ed.ac.uk, sam.lindley@@ed.ac.uk, philip.wadler@@ed.ac.uk}
\and
Chalmers University of Technology\\
\email{josefs@@chalmers.se}}

\maketitle

\begin{abstract} We describe a technique for engineering
domain-specific languages (DSLs) based on quotation and normalisation
of quoted terms, which we dub QDSL. We compare our technique to a
standard approach combining deep and shallow embedding, which we dub
CDSL. We draw attention to the importance of normalisation and the
subformula property for QDSLs in particular and DSLs in general. We
implement our system, and measure five benchmarks in QDSL and CDSL
implementations of Feldspar. \end{abstract}



\section{Introduction}

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
\citet{CheneyLW13}.  However, the rest of the QDSL approach---viewing
domain-specific languages as quoted terms---is widely used in other
systems, including Lisp macros, F\# and C\# LINQ \citep{fsharplinq,
csharplinq}, and Scala Lightweight Modular Staging (LMS)
\citep{scalalms}. In F\# LINQ quotation and anti-quotation are
explicit, as here, while in C\# LINQ and Scala LMS, quotation and
anti-quotation is controlled by type inference.

Feldspar exploits a combination of deep and shallow embedding, here
dubbed CDSL.  The technique is clearly described by
\citet{SvenningssonA12}, and further refined by \citet{PerssonAS11}
and \citet{SvenningssonS13}. Essentially the same technique is also
applied in Obsidian \citep{svensson2011obsidian} and Nikola
\citep{NIKOLA}.

In a single landmark paper, \citet{gentzen35} introduced the two
formulations of logic most widely used today, natural deduction and
sequent calculus, in both intuitionistic and classical variants.  (The
same paper introduced $\forall$ for universal quantification.)
Gentzen's main technical result was to establish the \emph{subformula}
property: any natural deduction proof may be put in a normal form
where all formulas it contains are subformulas of either its
hypotheses or conclusion. \citet{CheneyLW13} applied this result to
ensure queries with higher-order components always simplify to
first-order queries, easily translated to SQL.  Similarly here, our
QDSL source may refer to higher-order concepts or data types such as
|Maybe|, while we ensure that these do not appear in the generated
code.  The idea is not limited to QDSL, and the conclusion sketches
how to apply the same idea to CDSL.

%%
This paper makes the following contributions.
\begin{itemize}
  \item It introduces the name and the notion of QDSL.
  \item It introduces the name CDSL (but not the notion).
  \item It provide concise description and comparison of CDSL and QDSL.
  \item It observes how the subformula property applies to DSLs.
  \item It formalises a normalisation algorithm based on
        call-by-need reduction that ensures the subformula
        property while not losing sharing, and proves its correctness.
  \item It describes an implementation of Feldspar as a QDSL
  \item It compares Feldspar CDSL and QDSL on five benchmarks.
\end{itemize}
%%

The paper is organised as follows.
Section~\ref{sec:overview} introduces and compares the
CDSL and QDSL approaches, in the context of a simple example.
Section~\ref{sec:edsl} reviews how the CDSL approach
works in detail, in the context of Feldspar.
Section~\ref{sec:qdsl} describes how the QDSL approach
works in detail, reworking the examples of Section~\ref{sec:edsl}.
Section~\ref{sec:subformula} describes a normaliser
that ensures the subformula property while not losing sharing,
which can be applied to both call-by-need and call-by-value semantics.
Section~\ref{sec:evaluation} compares Feldspar CDSL and QDSL.
Section~\ref{sec:related} summarises related work.
Section~\ref{sec:conclusion} concludes.

\todo{Explain termination of normaliser and fix}

\section{Overview}
\label{sec:overview}

%include Section2.lhs

\section{MiniFeldspar as a CDSL}
\label{sec:edsl}

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
  \begin{itemize}
  \item For |(,)|, identifiers |fst| and |snd|.
  \item For |Maybe|, identifiers |return|, |(>>=)|, and |maybe|.
  \item For |Vec|, there are no relevant identifiers.
  \end{itemize}
\item It normalises the term to ensure the subformula property.
  Normalisation includes the special-purpose rules for |Maybe| and |Vec| given in
  Section~\ref{sec:qdsl} and the general-purpose rules of Section~\ref{sec:subformula}.
\item It traverses the term, converting |Qt| to |Dp|.
  It checks that only permitted primitives appear in |Qt|, and translates
  these to their corresponding representation in |Dp|. Permitted primitives include:
  \begin{itemize}
  \item |(==)| and |(<)|, treated as operations on integers.
  \item |(+)|, |(*)|, and other operations on integer and float.
  \item |while|, |arr|, |arrLen|, |arrIx|, which translate to
    |While|, |Arr|, |ArrLen|, and |ArrIx|.
  \end{itemize}
\end{itemize}

An unfortunate feature of typed quasiquotation in GHC is that the
implementation discards all type information when creating the
representation of a term.  Type |Qt a| is equivalent to
|TH.Q (TH.TExp a)|, where |TH| denotes the library for Typed Haskell,
|TH.Q| is the quotation monad of Typed Haskell (used to look up
identifiers and generate fresh names), and |TH.TExp a| is the parse
tree for an expression that returns a value of type |a|. In the
latter, |a| is a ghost variable; type |TH.TExp a| is a synonym for
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
languages.  Examples include \citep{reid1999prototyping,
hudak1997domain, bjesse1998lava}.

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

\section{Conclusion}
\label{sec:conclusion}

We have compared CDSLs and QDSLs, arguing that QDSLs offer competing
expressiveness and efficiency. CDSLs often (but not always) mimic the
syntax of the host language, and often (but not always) perform
normalisation in the host languages, while QDSLs (always) steal the
syntax of the host language, and (always) guarantee adequate
normalisation to ensure the subformula property, at the cost of
requiring a normaliser, one per host language.

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

Moliere's Monsieur Jordan was surprised to discover he had been
speaking prose his whole life. Similarly, many of us used QDSLs for
years, if not by that name. DSL via quotation is the heart of Lisp
macros, Microsoft LINQ, and Scala LMS, to name but three. We hope that
by naming the concept and drawing attention to the central benefits of
normalisation and the subformula propety, we may help the concept to
flower further for many more years to come.

\paragraph*{Acknowledgement} This work was funded by EPSRC Grant
EP/K034413/1.

\bibliographystyle{plainnat}
\bibliography{paper}

\end{document}
