\documentclass{llncs}

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
%format Opt_R
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

\todo{NBE citations - Sam}

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
ensure queries with higher-order components always simplify to
first-order queries, easily translated to SQL.  Similarly here, our
QDSL source may refer to higher-order concepts or data types such as
|Maybe|, while we ensure that these do not appear in the generated
code.  The idea is not limited to QDSL, as
Section~\ref{sec:edsl-maybe} applies the same idea to CDSL.

%%
The paper makes the following contributions.
\begin{itemize}
  \item introduce notion of QDSL
  \item introduce term CDSL (but not the concept)
  \item compare QDSL with CDSL
  \item observe that subformula property has general application to QDSLs and CDSLs
  \item formalisation of normaliser
  \item implementation of Feldspar as a QDSL
  \item evaluation of Feldspar CDSL and Feldspar QDSL
\end{itemize}
%%

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

\todo{Clarify what we really do about sharing}

\item Section~\ref{sec:empirical} presents empirical results for
Feldspar programs written and executed in both styles, showing CDSL
and QDSL achieve comparable results.

\end{itemize}
Section~\ref{sec:related} summarises related work, and
Section~\ref{sec:conclusion} concludes.

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
\label{sec:empirical}

\input{table}

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
