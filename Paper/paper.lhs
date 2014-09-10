\documentclass[preprint]{sigplanconf}

%include lhs2TeX.fmt
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
%% \usepackage[table]{xcolor}
%% \usepackage{colortbl}


%%% macros

\newcommand{\todo}[1]{{\noindent\small\color{red} \framebox{\parbox{\dimexpr\linewidth-2\fboxsep-2\fboxrule}{\textbf{TODO:} #1}}}}
%\newcommand{\todo}[1]{}

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
           {philip.wadler@@ed.ac.uk}

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
confirm that Feldspar and our technique yield identical target code on
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

