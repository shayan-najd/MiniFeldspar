%if False
\begin{code}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
import Prelude hiding (sqrt,min)
import Data.Array
type Arr a = Array Int a

min         ::  Ord a => Dp a ->  Dp a -> Dp a
min m n     =   (m .<. n) ? (m, n)

instance Fractional (Dp Float) where
  a / b          =  Prim2 "(/)" a b

sqrt :: (Syn a, Num a) => a -> Dp Float
sqrt e = Prim1 "Sqrt" (toDp e)

instance Num (Dp Float) where
  a + b  =  Prim2 "(+)" a b
  a - b  =  Prim2 "(-)" a b
  a * b  =  Prim2 "(*)" a b
  fromInteger a = LitF (fromInteger a)

\end{code}
%endif

%format returnn = return
%format >>==  = "\bind"

This section reviews the combination of deep and shallow embeddings
required to implement Feldspar as an EDSL (EFeldspar), and considers
the trade-offs between the QDSL and EDSL approaches.  Much of this
section reprises \citet{svenningsson:combining}.

\josef{This is the first time we use the name "EFeldspar". I suppose it should be introduced earlier.}
The top-level function of EFeldspar has the type:
\begin{spec}
edsl :: (Rep a , Rep b) => (Dp a -> Dp b) -> C
\end{spec}
\josef{Should it be |edsl| rather than |qdsl| above?}
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

In EDSL, no quotation is required, and the code looks almost---but not
quite!---like an unstaged version of power, but with different types.
Clever encoding tricks, explained later, permit declarations, function
calls, arithmetic operations, and numbers to appear the same whether
they are to be executed at generation-time or run-time.  However, as
explained later, comparison and conditionals appear differently
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
\shayan{Above does not read well. I think observable sharing is not a
technique by itself}
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
seamlessly. Depending on your point of view, explicit quotation syntax
may be considered as an unnecessary distraction or as drawing a useful
distinction between generation-time and run-time.  If one takes the
former view, the type-based approach to quotation found in C\# and
Scala might be preferred.
\shayan{That is, if the host language has proper overloading
machinery.  For example, Haskell doesn't hence type-based approach is
not possible sometimes. Moreover, we (at least I, Josef, and Dimitrios
from MSR) conjecture there is a theoretical limit to what overloading
can achieve, e.g. consider overloading the implicit fixpoint operator
in a recursive definition.}

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
preprocessor that checks this property. For example, in Haskell, it is
possible to incorporate such a preprocessor using MetaHaskell
\cite{metahaskell}.

\item Since QDSLs may share the same quoted terms across a range of
applications, the cost of building a normaliser or a preprocessor
might be amortised.  In the conclusion, we consider the design of a
tool for building QDSLs that may perform both these functions.

\item QDSL preserves sharing. In contrast, EDSL loses sharing, which
must later be recovered either by common subexpression elimination or
by applying a technique such as observable sharing.
\shayan{I think observable sharing is not a technique by itself.
I searched a bit, but could not find a proper phrase to replace it.}
\shayan{EDSL that does intensional analysis, like LMS, may preserve
sharing as we do. In fact, LMS has a layer transforming the input AST
to A-normal form, corresponding to our let insertion. Alas, EFeldspar,
and the combined technique loses sharing.}

\item Once the deep embedding or the normalised quoted term is
produced, generating the domain-specific code is similar for both
approaches.

\end{itemize}

\sam{A point not discussed here is code reuse between host and
  embedded languages. Links allows exactly the same code to be written
  once and used both in host and embedded code. QDSLs in Haskell do
  not (currently). F\# supports an attribute which can be used to
  reflect an arbitrary module as a quoted AST. What about other EDSL
  approaches?}

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
but excluding sums.
\shayan{There are multiple problems with above sentence:
(a) There are EFeldspar terms with product type that are not
    fully normalised when generated (e.g. refer to the complex
    numbers example that I wrote).
(b) Why "generating terms essentially in normal form" is attributed
    to EDSLs? As far as I know, nowhere in the combining deep and
    shallow embedding paper this was claimed.
(c) How do we know that there are no counter-examples even for
function types (i.e. generated terms of function type that are not in
normal form)?

I suggest removing "including functions and products,
but excluding sums", and fixing the following correspondingly.}

If one wishes to deal with sum types,
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
\shayan{I don't see expansion as normaliser's job; overloading
resolution happens in the translator. Though, I suppose it depends on
what we mean by normaliser.}

\item As this example shows, sometimes both QDSL and EDSL may require
normalisation.  As mentioned previously, for QDSLs the cost of
building a normaliser might be amortised across several applications.
In constrast, each EDSL usually has a distinct deep representation and
so requires a distinct normaliser.

\shayan{We should mention that deep represenations can use
compositional datatypes (e.g. Axelsson's Syntactic, or Scala's open
datatypes), and reuse a single normaliser.}

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
\shayan{One should add Rep constraint to the existential types. Types
are required for compiling terms to C.}

The type above represents a low level, pure functional language
with a straightforward translation to C. It uses higher-order
abstract syntax (HOAS) to represent constructs with variable binding
\citet{hoas}.

\sam{Technically this isn't true HOAS because it allows exotic terms.}

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
returnn    ::  a -> Opt_R a
returnn x  =   some_R x

(>>==)     ::  (Undef b) => Opt_R a -> (a -> Opt_R b) -> Opt_R b
o >>== g   =   Opt_R  (def o ? (def (g (val o)), false))
                      (def o ? (val (g (val o)), undef))
\end{code}
However, this adds type constraint |Undef b|
to the type of |(>>=)|, which is not permitted.
The need to add such constraints often arises, and has
been dubbed the constrained-monad problem
\citep{hughes1999restricted,SculthorpeBGG13,SvenningssonS13}.
We solve it with a trick due to \citet{PerssonAS11}.

\sam{I think Jeremy Yallop may have also covered this kind of issue
  somewhere in his thesis.}

We introduce a second continuation-passing style (CPS) type |Opt|,
defined in terms of the representation type |Opt_R|.  It is
straightforward to define |Monad| and |Syn| instances for the CPS
type, operations to lift the representation type to CPS and to lower
CPS to the representation type, and to lift |some|, |none|, and
|option| from the representation type to the CPS type.
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

normVec     ::  (Syn a, Num a) => Vec a -> Dp Float
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
         &  |dotVec u v|  \\
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
memorise (Vec n g)
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

EDSL silently converts between representations, while QDSL forces all
conversions to be written out; following the pattern that EDSL is more
compact, while QDSL is more explicit.  For QDSL it is the subformula
property which guarantees that all intermediate uses of |Vec| are
eliminated, while for EDSL this is established by operational
reasoning on the behaviour of the type |Vec|.
