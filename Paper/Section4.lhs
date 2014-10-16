%if False
\begin{code}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
type Qt a = Q (TExp a)
\end{code}
%endif

The CDSL technique is powerful. In a little over seven pages,
we explained sufficiently how to combinine deep and shallow
embedding to produce a library that allows all the CDSL code
in Section~\ref{sec:overview} to run.

We now review what is required to achieve the same effect in
QDSL. Although the treatment of CDSL is pleasingly compact, the
treatment of QDSL is even more compact.

For purposes of comparison, our CDSL and QDSL implementations both
produce abstract syntax trees for target code represented as terms of
type |Dp|. The postprocessor that converts |Dp| to C code is shared
among both implementations.

Thus, the key to our implementation is a transformer that converts
the representation of a quoted term |Qt| into type |Dp|.
\begin{spec}
translate :: Qt (a -> b) -> Dp a -> Dp b
\end{spec}
Composing |translate| with |cdsl|, the translator from |Dp| to C,
yields |qdsl|, our translator from quotations to C.
\begin{spec}
qdsl = cdsl . translate
\end{spec}
The processor translate is responsible for the following:
\begin{itemize}
\item It checks that only permitted primitives (those that can
  be translated to |Dp| and C) appear. Permitted primitives include:
  \begin{itemize}
  \item |(==)| and |(<)|, treated as operations on integers.
  \item |(+)|, |(*)|, and other operations on float.
  \item |while|, with the type described below.
  \item |ary|, |aryLen|, |aryIx|, with the type described below.
  \end{itemize}
\item It expands identifiers concerned with the types |(,)|, |Maybe|
  and |Ary|.
  \begin{itemize}
  \item For |(,)|, identifiers |fst| and |snd|.
  \item For |Maybe|, identifiers |return|, |(>>=)|, and |maybe|,
    as defined below.
  \item For |Ary|, identifiers |aryLen| and |aryIx|,
    as defined below.
  \end{itemize}
\end{itemize}
We describe below how a general purpose tool could be defined
to perform similar operations for any QDSL.


\subsection{While and for}

For CDSL, one had a deep construct and a corresponding ``smart
constructor'' exploiting class |Syntactic|.
\begin{spec}
While  ::  (Dp a -> Dp Bool) -> (Dp a -> Dp a) -> Dp a -> Dp a
while  ::  Syntactic a => (a -> Dp Bool) -> (a -> a) -> (a -> a)
\end{spec}
For QDSL, we follow the same pattern, but it's even simpler:
\begin{spec}
while  ::  (a -> Bool) -> (a -> a) -> (a -> a)
\end{spec}
This identifier is declared in the QDSL library with the appropriate
type, and conversion from |while| to |While| occurs when translating
|Qt| to |Dp|.

The definition of the |for| loop is given by:
\begin{code}
for  ::  Qt (Int -> a -> (Int -> a -> a) -> a)
for  =   [|| \n x_0 b -> snd (while (\(i,x) -> i < n) (\(i,x) -> (i+1) b i x) (0, x_0)) ||]
\end{code}
In other words, the code is similar in structure to that for CDSL, except
the type is simpler, quasi-quotes surround the body, and |(.<.)| in CDSL
may be replaced by |(<)| in QDSL.

An example of the use of |for| appears in Section~\ref{sec:qdsl-vec}.


\subsection{Embedding maybe}

The |Maybe| type is a part of the standard prelude.
\begin{spec}
data Maybe a  =   Nothing | Just a

return        ::  a -> Maybe a
return        =   Just

(>>=)         ::  Maybe a -> (a -> Maybe b) -> Maybe b
m >>= k       =   case m of
                    Nothing  ->  Nothing
                    Just x   ->  k x

maybe         ::  b -> (a -> b) -> Maybe a -> b
maybe x g m   =   case m of
                    Nothing  ->  x
                    Just y   ->  g y
\end{spec}
The QDSL processor permits the constructors
|Nothing| and |Just|, and replaces occurrences of |return|, |(>>=)|, and
|maybe| by the above definitions, prior to normalisation. The
normaliser performs simplifications including the rules below.
\begin{eqnarray*}
|case Nothing of { Nothing -> M; Just x -> N }| &\leadsto&  M  \\
|case Just L of {Nothing -> M; Just x -> N }|   &\leadsto&  N[x:=L]
\end{eqnarray*}
This is adequate to eliminate all occurrences of the |Maybe| type from
the code in Section~\ref{sec:second-example} after normalisation. The
subformula property is key: because the final type of the result does
not involve |Maybe|, it is certain that normalisation will remove all
its occurrences.


\subsection{Embedding array}

The |Vec| type is defined as follows.
\begin{code}
data Vec a  =  Vec Int (Int -> a)
\end{code}
The QDSL processor permits the constructor |Ary|, and performs
simplifications including the rule below.
\begin{eqnarray*}
|case Vec L M of { Vec n g -> N }| &\leadsto&  N[n:=L, g:=M]
\end{eqnarray*}
This is adequate to eliminate all occurrences of the |Vec| type from
the code below after normalisation. Again, the
subformula property is key: because the final type of the result does
not involve |Vec|, it is certain that normalisation will remove all
its occurrences.

Here are some primitive operators on vectors, in QDSL style.
\begin{code}
zipWithVec   ::  Qt ((a -> b -> c) -> Vec a -> Vec b -> Vec c)
zipWithVec   =   [||  \f (Vec m g) (Vec n h) ->
                        Vec ($$minim m n) (\i -> f (g i) (h i)) ||]

sumVec       ::  Num a => Qt (Vec a -> a)
sumVec       =   [|| \(Vec n g) -> $$for n 0 (\i x -> x + g i) ||]
\end{code}
This is identical to the previous code, save for additions of quotation and
anti-quotation.

Using our primitives, it is easy to compute the scalar product of two vectors.
\begin{code}
scalarProd   ::  Num a => Qt (Vec a -> Vec a -> a)
scalarProd   =   [|| \u v -> $$sumVec ($$zipWithVec (*) u v) ||]
\end{code}
With CDSL, evaluation in the host language achieves fusion.
With QDSL, normalisation in the QDSL processor achieves fusion.
As noted above, the subformula property is sufficient to guarantee
fusion is achieved.

Ultimately, we need to be able to manipulate actual arrays. For this
purpose, we provide three constants analogous to three constructs of
the type |Dp|.
\begin{spec}
ary      ::  Int -> (Int -> a) -> Array Int a
aryLen   ::  Array Int a -> Int
aryIx    ::  Array Int a -> Int -> a
\end{spec}
These identifiers are declared in the QDSL library with the appropriate
types, and conversion from |ary|, |aryLen|, |aryIx| to |Ary|,
|AryLen|, |AryIx|  occurs when translating |Qt| to |Dp|.

We convert between vectors and arrays as follows.
\begin{code}
toAry        ::  Qt (Vec a -> Ary a)
toAry        =   [|| \(Vec n g) -> ary n g ||]

fromAry      ::  Qt (Ary a -> Vec a)
fromAry      =   [|| \a -> Vec (aryLen a) (\i -> aryIx a i) ||]

memorise     ::  Qt (Vec a -> Vec a)
memorise     =   [|| fromAry . toAry ||]
\end{code}
The last performs the same purpose as |memorise| for CDSL,
enabling the user to decide when a vector is to be materialised
in memory.

\todo{Need an end-to-end use, to see where |toAry| and |fromAry| appear.}




