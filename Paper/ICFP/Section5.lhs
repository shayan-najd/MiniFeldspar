%if False
\begin{code}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
import Prelude
import Data.Array
type Arr a = Array Int a

instance Fractional (Dp Float) where
  a / b           =  Prim2 "(/)" a b
  fromRational f  =  LitF (fromRational f)

instance Floating (Dp Float) where
  sqrt e = Prim1 "Sqrt" (toDp e)

instance Num (Dp Float) where
  a + b  =  Prim2 "(+)" a b
  a - b  =  Prim2 "(-)" a b
  a * b  =  Prim2 "(*)" a b
  fromInteger a = LitF (fromInteger a)

data Typ a where
  Int :: Typ Int
  Flt :: Typ Float
  Bol :: Typ Bool
  Tpl :: Typ a -> Typ b -> Typ (a , b)
  Ary :: Typ a -> Typ (Arr a)

class Type a where
  typ :: Typ a

instance Type Int where
  typ = Int

instance Type Float where
  typ = Flt

instance Type Bool where
  typ = Bol

instance (Type a , Type b) => Type (a , b) where
  typ = Tpl typ typ

instance Type a => Type (Arr a) where
  typ = Ary typ

class (Type a) => Rep a where
instance Rep Int
instance Rep Float
instance Rep Bool
instance (Rep a , Rep b) => Rep (a , b)
instance Rep a => Rep (Arr a)

\end{code}
%endif

%format returnn = return
%format >>==  = "\bind"

This section reviews the combination of deep and shallow embeddings
required to implement Feldspar as an EDSL, and considers
the trade-offs between the QDSL and EDSL approaches.  Much of this
section reprises \citet{svenningsson:combining}.

The top-level function of EDSL Feldspar has the type:
\begin{spec}
edsl :: (Rep a , Rep b) => (Dp a -> Dp b) -> C
\end{spec}
Here |Dp a| is the deep representation of a term of type |a|.
The deep representation is described in detail in Section~\ref{subsec:deep}
below, and is chosen to be easy to translate to C.
As before, type |C| represents code in C,
and type class |Rep| restricts to representable types.

% We write |Dp a| is the deep representation of a term of type |a|,
% as described in Section~\ref{subsec:deep}. Terms of the deep
% representation are chosen to be easy to translate to C.
% The top-level function of EDSL Feldspar has the type:
% \begin{spec}
% edsl :: (Syn a , Syn b) => (a -> b) -> C
% \end{spec}
% Here |Syn a| restricts |a| to be a type that can be converted from
% or to a representable type, as described in Section~\ref{subsec:syn}.
% In particular, it has as instances |Syn (Dp a)|, whenever |a| is a
% representable type. As before, type |C| represents code in C.


\subsection{A first example}
\label{subsec:e-power}

Here is the power function of Section~\ref{subsec:power},
written as an EDSL:
\begin{code}
power :: Int -> Dp Float -> Dp Float
power n x  =
  if n < 0 then
    x .==. 0 ? (0,  1 / power (-n) x)
  else if n == 0 then
    1
  else if even n then
    let y = power (n `div` 2) x in y * y
  else
    x * power (n-1) x
\end{code}
Type |Q (Float -> Float)| in the QDSL variant becomes the type
|Dp Float -> Dp Float| in the EDSL variant, meaning that |power n|
accepts a representation
of the argument and returns a representation of that argument raised
to the $n$'th power.

In the EDSL variant, no quotation is required, and the code looks
almost---but not quite!---like an unstaged version of power, but with
different types.  Clever encoding tricks, explained later, permit
declarations, function calls, arithmetic operations, and numbers to
appear the same whether they are to be executed at generation-time or
run-time.  However, as explained later, comparison and conditionals
appear differently depending on whether they are to be executed at
generation-time or run-time, using |M == N| and |if L then M else N|
for the former but |M .==. N| and |L ? (M, N)| for the latter.

Evaluating |power (-6)| yields the following:
\begin{spec}
(\ u -> (u .==. 0) ? (0,
  1 / (  (u * ((u * 1) * (u * 1))) *
         (u * ((u * 1) * (u * 1))))))
\end{spec}
Applying common-subexpression elimination, for instance
via observable sharing \citep{claessen1999observable,gill2009type},
permits recovering the sharing structure.
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
identical.  In contrast, in Haskell, EDSL requires some term forms,
such as comparison and conditionals, to differ between the host and
embedded languages.  Other languages, notably Scala Virtualised
\citep{rompf2013scala}, may support more general overloading that
allows even comparison and conditionals to be identical.

\item QDSL requires syntax to separate quoted and unquoted terms. In
contrast, EDSL permits the host and embedded languages to intermingle
seamlessly. Depending on your point of view, explicit quotation syntax
may be considered as an unnecessary distraction or as drawing a useful
distinction between generation-time and run-time.  If one takes the
former view, the type-based approach to quotation found in C\# and
Scala might be preferred.

\item QDSL may share the same representation for quoted terms across a
range of applications; the quoted language is the host language, and
does not vary with the specific domain.  In contrast, EDSL typically
develops custom shallow and deep embeddings for each application;
a notable exception is the LMS and Delite frameworks for Scala,
which provide a deep embedding shared across several disparate DSLs
\citep{sujeeth2013composition}.

\item QDSL yields an unwieldy term that requires normalisation.  In
contrast, EDSL yields the term in normalised form in this case, though
there are other situations where a normaliser is required (see
Section~\ref{subsec:e-maybe}).

\item QDSL requires traversing the quoted term to ensure it only
mentions permitted identifiers. In contrast, EDSL guarantees that if a
term has the right type it will translate to the target.  If the
requirement to eyeball code to ensure only permitted identifiers are
used is considered too onerous, it should be easy to build a
preprocessor that checks this property. For example, in Haskell, it is
possible to incorporate such a preprocessor using MetaHaskell
\cite{metahaskell}.

\item Since QDSLs may share the same quoted terms across a range of
applications, the cost of building a normaliser or a preprocessor
might be amortised across multiple QDSLs for a single language.  In
the conclusion, we consider the design of a tool for building QDSLs
that uses a shared normaliser and preprocessor.

% \item QDSL preserves sharing. In contrast, EDSL loses sharing, which
% must later be recovered either by common subexpression elimination or
% by applying a technique such as observable sharing
% \citep{claessen1999observable, gill2009type}.  EDSL with common
% subexpression elimination may discover common subexpressions that are
% distinct in QDSL; while EDSL with observable sharing yields
% results comparable to sharing-preserving QDSL.

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
        return (y * y)
  else
    do  y <- power' (n-1) x
        return (x*y)

power'' ::  Int -> Dp Float -> Dp Float
power'' n x  =  option 0 (\y -> y) (power' n x)
\end{code}
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
as described in Section~\ref{sec:implementation}.
After applying these rules, common subexpression
elimination yields the same structure as in the previous subsection,
from which the same C code is generated.

Here we have described normalisation via rewriting, but some EDSLs
achieve normalisation via smart constructors,
which ensure deep terms are always in normal form
\citep{rompf2012lightweight}; the two techniques are roughly equivalent.

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

Hence, an advantage of the EDSL approach---that it generates terms
essentially in normal form---turns out to apply sometimes but not
others. It appears to often work for functions and products, but to
fail for sums.  In such situations, separate normalisation is
required. This is one reason why we do not consider normalisation as
required by QDSL to be particularly onerous.

Here are points of comparison between the two approaches.
\begin{itemize}

\item Both QDSL and EDSL can exploit notational conveniences in the
host language. The example here exploits Haskell |do| notation; the
embedding of SQL in F\# by \citet{cheney:linq} expoits F\# sequence
notation. For EDSL, exploiting |do| notation just requires
instantiating |return| and |(>>=)| correctly. For QDSL, it is
also necessary for the translator to recognise and expand
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
  Fst	    ::  Rep b => Dp (a,b) -> Dp a
  Snd       ::  Rep a => Dp (a,b) -> Dp b
  Prim1	    ::  Rep a => String -> Dp a -> Dp b
  Prim2     ::  (Rep a , Rep b) =>
                String -> Dp a -> Dp b -> Dp c
  MkArr     ::  Dp Int -> (Dp Int -> Dp a) -> Dp (Arr a)
  LnArr     ::  Rep a => Dp (Arr a) -> Dp Int
  IxArr     ::  Dp (Arr a) -> Dp Int -> Dp a
  Save      ::  Dp a -> Dp a
  Let       ::  Rep a => Dp a -> (Dp a -> Dp b) -> Dp b
  Variable  ::  String -> Dp a
\end{code}
% Tag       ::  String -> Dp a -> Dp a
Type |Dp| represents a low level, pure functional language
with a straightforward translation to C. It uses higher-order
abstract syntax (HOAS) to represent constructs with variable binding
\citet{hoas}.
Our code obeys the invariant that we only write |Dp a|
when |Rep a| holds, that is, when type |a| is representable.

The deep embedding has boolean, integer, and floating point literals,
conditionals, while loops, pairs, primitives, arrays,
and special-purpose constructs to disable normalisation,
for let binding, and for variables.
Constructs |LitB|, |LitI|, |LitF| build literals;
|If| builds a conditional.
|While| corresponds to |while| in Section~\ref{subsec:while};
|Pair|, |Fst|, and |Snd| build and decompose pairs;
|Prim1| and |Prim2| represent primitive
operations, where the string is the name of the operation;
|MkArr|, |LnArr|, and |IxArr|
correspond to the array operations in Section~\ref{subsec:arrays};
|Save| corresponds to |save| in Section~\ref{subsec:subformula};
|Let| corresponds to let binding,
% |Tag| indicates common subexpressions,
and |Variable| is used when translating HOAS to C code.


\subsection{Class |Syn|}
\label{subsec:syn}

We introduce a type class |Syn| that allows us to convert
shallow embeddings to and from deep embeddings.
\begin{code}
class Rep (Internal a) => Syn a where
  type Internal a
  toDp    ::  a -> Dp (Internal a)
  fromDp  ::  Dp (Internal a) -> a
\end{code}
Type |Internal| is a GHC type family \citep{type-families}.  Functions
|toDp| and |fromDp| translate between the shallow embedding |a| and the
deep embedding |Dp (Internal a)|.

The first instance of |Syn| is |Dp| itself, and is straightforward.
\begin{code}
instance Rep a => Syn (Dp a) where
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
% \shayan{We need to mention the argument that overriding do actually
% apply. But, the problem with overriding is that once a syntactic
% entity is fixed to be used i one stage, it cannot be reused in other
% stage.}

Here is how to compute the minimum of two values.
\begin{code}
minim         ::  (Syn a, Ord (Internal a)) => a -> a -> a
minim x y     =   (x .<. y) ? (x, y)
\end{code}


\subsection{Embedding pairs}

Host language pairs in the shallow embedding
correspond to target language pairs in the deep embedding.
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
% \shayan{Though I heard you all say otherwise, but I still find above
% provocative (for the reader / reviewer).}


\subsection{Embedding option}
\label{subsec:opt}

We now explain in detail the |Opt| type seen in Section~\ref{subsec:maybe}.

The deep-and-shallow technique represents deep embeddding
|Dp (a,b)| by shallow embedding |(Dp a, Dp b)|.  Hence, it is tempting to
represent |Dp (Maybe a)| by |Maybe (Dp a)|, but this cannot work,
because |fromDp| would have to decide at generation-time whether to
return |Just| or |Nothing|, but which to use is not known until
run-time.

Instead, \citet{svenningsson:combining} represent values of type
|Maybe a| by the type |Opt' a|, which pairs a boolean with a value of
type |a|.  For a value corresponding to |Just x|, the boolean is true
and the value is |x|, while for one corresponding to |Nothing|, the
boolean is false and the value is |undef|.  We define |some'|,
|none'|, and |option'| as the analogues of |Just|, |Nothing|, and
|maybe|.  The |Syn| instance is straightforward, mapping options to
and from the pairs already defined for |Dp|.
\begin{code}
data Opt' a = Opt' { def :: Dp Bool, val :: a }

instance Syn a => Syn (Opt' a) where
  type Internal (Opt' a) = (Bool, Internal a)
  toDp (Opt' b x)  =  Pair b (toDp x)
  fromDp p         =  Opt' (Fst p) (fromDp (Snd p))

some'          ::  a -> Opt' a
some' x        =   Opt' true x

none'          ::  Undef a => Opt' a
none'          =   Opt' false undef

option'        ::  Syn b => b -> (a -> b) -> Opt' a -> b
option' d f o  =   def o ? (f (val o), d)
\end{code}

The next obvious step is to define a suitable monad over the type |Opt'|.
The natural definitions to use are as follows:
\begin{spec}
return    ::  a -> Opt' a
return x  =   some' x

(>>=)     ::  (Undef b) => Opt' a -> (a -> Opt' b) -> Opt' b
o >>= g   =   Opt'  (def o ? (def (g (val o)), false))
                    (def o ? (val (g (val o)), undef))
\end{spec}
However, this adds type constraint |Undef b|
to the type of |(>>=)|, which is not permitted.
The need to add such constraints often arises, and has
been dubbed the constrained-monad problem
\citep{hughes1999restricted,SvenningssonS13}.
% SculthorpeBGG13
We solve it with a trick due to \citet{PerssonAS11}.

% \sam{I think Jeremy Yallop may have also covered this kind of issue
%   somewhere in his thesis.}

We introduce a continuation-passing style (CPS) type, |Opt|,
defined in terms of |Opt'|.  It is
straightforward to define |Monad| and |Syn| instances,
operations to lift the representation type to lift and lower
one type to the other, and to lift |some|, |none|, and
|option| to the CPS type.
The |lift| operation is closely related to the |(>>=)| operation
we could not define above; it is properly typed,
thanks to the type constraint on |b| in the definition of |Opt a|.

\begin{code}
newtype Opt a =
  O { unO :: forall b . Undef b => ((a -> Opt' b) -> Opt' b) }

instance Monad Opt where
  return x    =  O (\g -> g x)
  m >>= k     =  O (\g -> unO m (\x -> unO (k x) g))

instance Undef a => Syn (Opt a) where
  type Internal (Opt a) = (Bool, Internal a)
  fromDp  =  lift . fromDp
  toDp    =  toDp . lower

lift :: Opt' a -> Opt a
lift o =  O (\g -> Opt'  (def o ? (def (g (val o)), false))
                         (def o ? (val (g (val o)), undef)))

lower :: Undef a => Opt a -> Opt' a
lower m = unO m some'

none :: Undef a => Opt a
none =  lift none'

some :: a -> Opt a
some a = lift (some' a)

option :: (Undef a, Syn b) => b -> (a -> b) -> Opt a -> b
option d f o = option' d f (lower o)
\end{code}
These definitions support the EDSL code presented
in Section~\ref{subsec:e-maybe}.


\subsection{Embedding vector}

Recall that values of type |Array| are created by construct |MkArr|,
while |LnArr| extracts the length and |IxArr| fetches the element at
the given index.  Corresponding to the deep embedding |Array| is a
shallow embedding |Vec|.
\begin{code}
data Vec a = Vec (Dp Int) (Dp Int -> a)

instance Syn a => Syn (Vec a) where
  type Internal (Vec a) = Array Int (Internal a)
  toDp (Vec n g)    =  MkArr n (toDp . g)
  fromDp a          =  Vec  (LnArr a) (fromDp . IxArr a)

instance Functor Vec where
  fmap f (Vec n g)  =  Vec n (f . g)
\end{code}
Constructor |Vec| resembles |Arr|, but the former
constructs a high-level representation of the array and the latter an
actual array.
It is straightforward to make |Vec| an instance of |Functor|.

It is easy to define operations on vectors,
including combining corresponding elements of two vectors,
summing the elements of a vector, dot product of two vectors,
and norm of a vector.
\begin{code}
zipVec  ::  (Syn a, Syn b) =>
              (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipVec f (Vec m g) (Vec n h)
        =   Vec (m `minim` n) (\i -> f (g i) (h i))

sumVec  ::  (Syn a, Num a) => Vec a -> a
sumVec (Vec n g)
        =   for n 0 (\i x -> x + g i)

dotVec      ::  (Syn a, Num a) => Vec a -> Vec a -> a
dotVec u v  =   sumVec (zipVec (*) u v)

normVec     ::  Vec (Dp Float) -> Dp Float
normVec v   =   sqrt (dotVec v v)
\end{code}
Invoking
\[
|edsl (normVec . toVec)|
\]
generates C code to normalise a vector. If we used a top-level function
of type |(Syn a, Syn b) => (a -> b) -> C|, then it would insert the
|toVec| coercion automatically.

This style of definition again provides fusion. For instance:
\[
\begin{array}{cl}
  &  |dotVec (Vec m g) (Vec n h)|  \\
= &  |sumVec (zipVec (*) (Vec m g) (Vec n h)|  \\
= &  |sumVec (Vec (m `minim` n) (\i -> g i * h i)|  \\
= &  |for (m `minim` n) (\i x -> x + g i * h i)|
\end{array}
\]
Indeed, we can see that by construction that whenever we combine two
primitives the intermediate vector is always eliminated.

The type class |Syn| enables conversion between types |Arr| and |Vec|.
Hence for EDSL, unlike QDSL, explicit calls |toVec| and |fromVec| are
not required.  Invoking |edsl normVec| produces the same C code as in
Section~\ref{subsec:arrays}.

As with QDSL, there are some situations where fusion is not beneficial.
We may materialise a vector as an array with the following function.
\begin{code}
memorise :: Syn a => Vec a -> Vec a
memorise = fromDp . Save . toDp
\end{code}
% The above definition depends on common subexpression elimination
% to ensure |Arr n (toDp . g)| is computed once, rather than once
% for each element of the resulting vector.
Here we interpose |Save| to forestall the fusion that would otherwise occur.
For example, if
\begin{spec}
blur :: Syn a => Vec a -> Vec a
blur v = zipVec  (\x y -> sqrt (x*y))
                 (appVec a (uniVec 0))
                 (appVec (uniVec 0) a)
\end{spec}
computes the geometric mean of adjacent elements of a vector, then one may choose to
compute either
\begin{center}
|blur . blur| ~~~or~~~ |blur . memorise . blur|
\end{center}
with different trade-offs between recomputation and memory use.

QDSL forces all conversions to be written out, while EDSL silently
converts between representations; following the pattern that QDSL is
more explicit, while EDSL is more compact.  For QDSL it is the
subformula property which guarantees that all intermediate uses of
|Vec| are eliminated, while for EDSL this is established by
operational reasoning on the behaviour of the type |Vec|.
