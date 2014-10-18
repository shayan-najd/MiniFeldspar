%if False
\begin{code}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
type Qt a = Q (TExp a)
\end{code}
%endif

In under eight pages, we explained how to combine deep and
shallow embedding to produce a library that allows all CDSL code in
Section~\ref{sec:overview} to run.  Now, in three pages, we will
cover what is required to achieve the same effect in QDSL.

Our CDSL and QDSL implementations both represent target code as terms
of type |Dp|. The postprocessor that converts |Dp| to C code is shared
among both implementations.  Central to our implementation is a
translator that converts the representation of a quoted term |Qt| into
type |Dp|.  Prior to translation, terms of |Qt| are normalised to
ensure the subformula property.

\subsection{While and for}

For CDSL, we had a deep construct and corresponding ``smart
constructor''.
\begin{spec}
While  ::  (Dp a -> Dp Bool) -> (Dp a -> Dp a) -> Dp a -> Dp a
while  ::  Syn a => (a -> Dp Bool) -> (a -> a) -> (a -> a)
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
The code is similar in structure to that for CDSL, except
the type is simpler, quasi-quotes surround the body, and |(.<.)| in CDSL
is replaced by |(<)| in QDSL.


\subsection{Embedding maybe}

The |Maybe| type is a part of the standard prelude.
\begin{spec}
data Maybe a  =   Nothing | Just a

return        ::  a -> Maybe a
return        =   Just

(>>=)         ::  Maybe a -> (a -> Maybe b) -> Maybe b
m >>= k       =   case m of { Nothing -> Nothing ; Just x -> k x }

maybe         ::  b -> (a -> b) -> Maybe a -> b
maybe x g m   =   case m of { Nothing -> x ; Just y -> g y }
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
The QDSL processor permits the constructor |Arr|, and performs
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
arr      ::  Int -> (Int -> a) -> Array Int a
arrLen   ::  Array Int a -> Int
arrIx    ::  Array Int a -> Int -> a
\end{spec}
These identifiers are declared in the QDSL library with the appropriate
types, and conversion from |arr|, |arrLen|, |arrIx| to |Arr|,
|ArrLen|, |ArrIx|  occurs when translating |Qt| to |Dp|.

We convert between vectors and arrays as follows.
\begin{code}
toArr        ::  Qt (Vec a -> Array Int a)
toArr        =   [|| \(Vec n g) -> arr n g ||]

fromArr      ::  Qt (Array Int a -> Vec a)
fromArr      =   [|| \a -> Vec (arrLen a) (\i -> arrIx a i) ||]
\end{code}
If we define
\begin{code}
norm     ::  Qt (Vec Float -> Float)
norm v   =   scalarProd v v
\end{code}
invoking |qdsl (norm . fromArr)| yields C code that accepts
an array. In contrast to CDSL, the coercion must be inserted
by hand. The advantage is that whereas for CDSL we must resort
to operational reasoning to determine which instances of |Vec|
are eliminated, with QDSL we can apply the subformula property.
We know that all occurrences of |Vec| must be
eliminated since it appears as neither an input nor output
of the final program.

Defining an analogue of |memorise| is straightforward.
\begin{code}
memorise     ::  Qt (Vec a -> Vec a)
memorise     =   [|| fromArr . toArr ||]
\end{code}
The last performs the same purpose as |memorise| for CDSL,
enabling the user to decide when a vector is to be materialised
in memory.




