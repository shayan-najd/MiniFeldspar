%if False
\begin{code}
{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, Rank2Types #-}
module Section3 where
import Data.Array
\end{code}
%endif

We now review the usual approach to embedding a DSL into
a host language by combining deep and shallow embedding.
As a running example, we will use MiniFeldspar, an embedded DSL for
generating signal processing software in C.
Much of this section reprises \citet{SvenningssonA12}.

\subsection{The deep embedding}

Recall that a value of type |Dp a| represents a term of type |a|,
and is called a deep embedding.
\begin{code}
data Dp a where
  LitB		::  Bool -> Dp Bool
  LitI		::  Int -> Dp Int
  LitF		::  Float -> Dp Float
  If		::  Dp Bool -> Dp a -> Dp a -> Dp a
  While		::  (Dp a -> Dp Bool) -> (Dp a -> Dp a) -> Dp a -> Dp a
  Pair		::  Dp a -> Dp b -> Dp (a,b)
  Fst		::  Dp (a,b) -> Dp a
  Snd           ::  Dp (a,b) -> Dp b
  Prim1		::  String -> (a -> b) -> Dp a -> Dp b
  Prim2         ::  String -> (a -> b -> c) -> Dp a -> Dp b -> Dp c
  Arr           ::  Dp Int -> (Dp Int -> Dp a) -> Dp (Array Int a)
  ArrLen        ::  Dp (Array Int a) -> Dp Int
  ArrIx         ::  Dp (Array Int a) -> Dp Int -> Dp a
  Variable      ::  String -> Dp a
  Value         ::  a -> Dp a
\end{code}
The type above represents a low level, pure functional language
with a straightforward translation to C. It uses higher-order
abstract syntax (HOAS) to represent constructs with variable binding
\citet{hoas}.

Our CDSL has boolean, integer, and floating point literals,
conditionals, while loops, pairs, primitives, arrays,
and special-purpose constructs for variables and values.
Constructs |LitB|, |LitI|, |LitF| build literals.
Construct |If| builds a conditional.
Construct |While| may require explanation.
Rather than using side-effects, the while loop takes
three arguments, a function from current state |a| to a boolean,
and a function from current state |a| to new state |a|,
and initial state |a|, and returns final state |a|.
Constructs |Pair|, |Fst|, and |Snd| build pairs and
extract the first and second component.
Constructs |Prim1| and |Prim2| represent primitive
operations, the string is the name of the operation
(used in printing or to generate C) and the function
argument computes the primitive (used in evaluation).
Construct |ArrIx| creates a new array from a length and
oa body that computes the array element
for each index, construct |ArrLen| extracts the length from an array, and
construct |ArrIx| fetches the element at a given index.
Construct |Variable| is used in printing and in generating C,
construct |Value| is used in the evaluator.

The exact semantics is given by |eval|. It is a strict language,
so we define an infix strict application operator |(<*>)|.
\begin{code}
(<*>)                 ::  (a -> b) -> a -> b
f <*> x               =   seq x (f x)

eval                  ::  Dp a -> a
eval (LitI i)         =   i
eval (LitF x)         =   x
eval (LitB b)         =   b
eval (If c t e)       =   if eval c then eval t else eval e
eval (While c b i)    =   evalWhile (evalFun c) (evalFun b) <*> eval i
eval (Pair a b)       =   (,) <*> eval a <*> eval b
eval (Fst p)          =   fst <*> eval p
eval (Snd p)          =   snd <*> eval p
eval (Prim1 _ f a)    =   f <*> eval a
eval (Prim2 _ f a b)  =   f <*> eval a <*> eval b
eval (Arr n g)        =   array (0,n') [ (i, eval (g (LitI i))) | i <- [0..n'] ]
                          where n' = eval n - 1
eval (ArrLen a)       =   u-l+1  where (l,u) = bounds (eval a)
eval (ArrIx a i)      =   eval a ! eval i
eval (Value a)        =   a

evalFun               ::  (Dp a -> Dp b) -> a -> b
evalFun f x           =   (eval . f . Value) <*> x

evalWhile             ::  (a -> Bool) -> (a -> a) -> a -> a
evalWhile c b i       =   if c i then evalWhile c b (b i) else i
\end{code}
Function |eval| plays no role in generating C, but may be useful for testing.


\subsection{Class |Syntactic|}

We introduce a type class |Syntactic| that allows us to convert
shallow embeddings to and from deep embeddings.
\begin{code}
class Syntactic a where
  type Internal a
  toDp    	   ::  a -> Dp (Internal a)
  fromDp  	   ::  Dp (Internal a) -> a
\end{code}
Type |Internal| is a GHC type family \citep{type-families}.  Functions
|toDp| and |fromDp| translate between the shallow embedding |a| and the
deep embedding |Dp (Internal a)|.

The first instance of |Syntactic| is |Dp| itself, and is straightforward.
\begin{code}
instance Syntactic (Dp a) where
  type Internal (Dp a)  =  a
  toDp    	        =  id
  fromDp  	        =  id
\end{code}
Our representation of a run-time |Bool| will have type |Dp Bool| in
both the deep and shallow embeddings, and similarly for |Int| and
|Float|.

We do not code the target language using its constructors
directly. Instead, for each constructor we define a corresponding
``smart constructor'' using class |Syntactic|.
\begin{code}
true, false  ::  Dp Bool
true         =   LitB True
false        =   LitB False

(?)          ::  Syntactic a => Dp Bool -> (a,a) -> a
c ? (t,e)    =   fromDp (If c (toDp t) (toDp e))

while        ::  Syntactic a => (a -> Dp Bool) -> (a -> a) -> a -> a
while c b i  =   fromDp (While (c . fromDp) (toDp . b . fromDp) (toDp i))
\end{code}

Numbers are made convenient to manipulate via overloading.
\begin{code}
instance Num (Dp Int) where
  a + b          =  Prim2 "(+)" (+) a b
  a - b          =  Prim2 "(-)" (-) a b
  a * b          =  Prim2 "(*)" (*) a b
  fromInteger a  =  LitI (fromInteger a)
\end{code}
With this declaration, |1+2 :: Dp Int| evaluates to
|Prim2 "(+)" (+) (LitI 1) (LitI 2)|, permitting code
executed at generation-time and run-time to appear identical.
A similar declaration works for |Float|.
%if False
\begin{code}
instance Num (Dp Float) where
  a + b          =  Prim2 "(+)" (+) a b
  a - b          =  Prim2 "(-)" (-) a b
  a * b          =  Prim2 "(*)" (*) a b
  fromInteger a  =  LitF (fromInteger a)

instance Fractional (Dp Float) where
  a / b          =  Prim2 "(/)" (/) a b
\end{code}
%endif

Comparison also benefits from smart constructors.
\begin{code}
(.==.)       ::  (Syntactic a, Eq (Internal a)) => a -> a -> Dp Bool
a .==. b     =   Prim2 "(==)" (==) (toDp a) (toDp b)

(.<.)        ::  (Syntactic a, Ord (Internal a)) => a -> a -> Dp Bool
a .<. b      =   Prim2 "(<)" (<) (toDp a) (toDp b)
\end{code}
Overloading cannot apply here, because Haskell requires
|(==)| return a result of type |Bool|, while |(.==.)| returns
a result of type |Dp Bool|, and similarly for |(.<.)|.

Here is how to compute the minimum of two values.
\begin{code}
minim         ::  Ord a => Dp a ->  Dp a -> Dp a
minim m n     =   (m .<. n) ? (m, n)
\end{code}


\subsection{Embedding pairs}

We set up a correspondence between host language pairs
in the shallow embedding and target language pairs in the deep embedding.
\begin{code}
instance (Syntactic a, Syntactic b) => Syntactic (a,b) where
  type  Internal (a,b)  =  (Internal a, Internal b)
  toDp (a,b)            =  Pair (toDp a) (toDp b)
  fromDp p              =  (fromDp (Fst p), fromDp (Snd p))
\end{code}
This permits us to manipulate pairs as normal, with |(a,b)|, |fst a|,
and |snd a|.  (Argument |p| is duplicated in the definition of
|fromDp|, which may require common subexpression elimination or
observable sharing, as discussed in Section~\ref{sec:first-example}.)

We have now developed sufficient machinery to define a |for| loop
in terms of a |while| loop.
\begin{code}
for          ::  Syntactic a => Dp Int -> a -> (Dp Int -> a -> a) -> a
for n x_0 b  =   snd (while (\(i,x) -> i .<. n) (\(i,x) -> (i+1, b i x)) (0,x_0))
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
value at each type, which we call |undef|.

% (A better name might be `default' or `undefined', if each did not
% already have another meaning in Haskell.)

It is straightforward to define a type class |Undef|, where type |a|
belongs to |Undef| if it belongs to |Syntactic| and it has an
undefined value.
\begin{code}
class Syntactic a => Undef a where
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

\citet{SvenningssonA12} claim that it is not possible to support
|undef| without changing the deep embedding, but here we have defined |undef|
entirely as a shallow embedding.  (It appears they underestimated the
power of their own technique!)


\subsection{Embedding option}

We now explain in detail the |Option| type seen in Section~\ref{sec:second-example}.

The deep-and-shallow technique cleverly represents deep embeddding
|Dp (a,b)| by shallow embedding |(Dp a, Dp b)|.  Hence, it is tempting to
represent |Dp (Maybe a)| by |Maybe (Dp a)|, but this cannot work,
because |fromDp| would have to decide at generation-time whether to
return |Just| or |Nothing|, but which to use is not known until
run-time.

Indeed, rather than extending the deep embedding to support the type
|Dp (Maybe a)|, \citet{SvenningssonA12} prefer a different choice, that
represents optional values while leaving |Dp| unchanged.  Following
their development, we represent values of type |Maybe a| by the type
|Opt_R a|, which pairs a boolean with a value of type |a|.  For a
value corresponding to |Just x|, the boolean is true and the value is
|x|, while for one corresponding to |Nothing|, the boolean is false
and the value is |undef|.  We define |some_R|, |none_R|, and |opt_R|
as the analogues of |Just|, |Nothing|, and |maybe|.  The |Syntactic|
instance is straightforward, mapping options to and from the pairs
already defined for |Dp|.
\begin{code}
data Opt_R a = Opt_R { def :: Dp Bool, val :: a }

instance Syntactic a => Syntactic (Opt_R a) where
  type Internal (Opt_R a)  =  (Bool, Internal a)
  toDp (Opt_R b x)         =  Pair b (toDp x)
  fromDp p                 =  Opt_R (Fst p) (fromDp (Snd p))

some_R            ::  a -> Opt_R a
some_R x          =   Opt_R true x

none_R            ::  Undef a => Opt_R a
none_R            =   Opt_R false undef

option_R          ::  Syntactic b => b -> (a -> b) -> Opt_R a -> b
option_R d f o    =   def o ? (f (val o), d)
\end{code}

The next obvious step is to define a suitable monad over the type |Opt_R|.
The natural definitions to use are as follows.
\begin{code}
return    ::  a -> Opt_R a
return x  =   some_R x

(>>=)     ::  (Undef b) => Opt_R a -> (a -> Opt_R b) -> Opt_R b
o >>= g   =   Opt_R  (def o ? (def (g (val o)), false))
                     (def o ? (val (g (val o)), undef))
\end{code}
However, this adds type constraint |Undef b|
to the type of |(>>=)|, which is not permitted.
This need to add constraints often arises, and has
been dubbed the constrained-monad problem
\citet{hughes:restricted-monad,SculthorpeBGG13,SvenningssonS13}.
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
  return x    =  O (\g -> g x)
  m >>= k     =  O (\g -> unO m (\x -> unO (k x) g))

instance Undef a => Syntactic (Opt a) where
  type Internal (Opt a)  =  (Bool, Internal a)
  fromDp                  =  lift . fromDp
  toDp                    =  toDp . lower

lift          ::  Opt_R a -> Opt a
lift o        =   O (\g -> Opt_R  (def o ? (def (g (val o)), false))
                                  (def o ? (val (g (val o)), undef)))

lower         ::  Undef a => Opt a -> Opt_R a
lower m       =   unO m some_R

some          ::  a -> Opt a
some a        =   lift (some_R a)

none          ::  Undef a => Opt a
none          =   lift none_R

option        ::  (Undef a, Undef b) => b -> (a -> b) -> Opt a -> b
option d f o  =   option_R d f (lower o)
\end{code}

These definitions are adequate to support the CDSL code presented
in Section~\ref{sec:second-example}.

\todo{After discussing the subformula property, a different solution
to this problem becomes available.}


\subsection{Embedding vector}

Array programming is central to the intended application domain
of MiniFeldspar. In this section, we extend our CDSL to handle
arrays.

Recall that values of type |Array| are created by construct |Arr|,
while |ArrLen| extracts the length and |ArrIx| fetches the element at
the given index.  Corresponding to the deep embedding |Array| is a
shallow embedding |Vector|.
\begin{code}
data Vector a = Vec (Dp Int) (Dp Int -> a)

instance Syntactic a => Syntactic (Vector a) where
  type Internal (Vector a)  =  Array Int (Internal a)
  toDp (Vec n g)            =  Arr n (toDp . g)
  fromDp a                  =  Vec (ArrLen a) (\i -> fromDp (ArrIx a i))

instance Functor Vector where
  fmap f (Vec n g)          =  Vec n (f . g)
\end{code}
The shallow embedding |Vec| resembles the constructor |Arr|, but whereas
the body of |Arr| must return values of type |Dp a|, the body of a vector
may return values of any type |a| that satisfies |Syntactic a|.
It is straightforward to make |Vector| an instance of |Functor|.

Here are some primitive operations on vectors
\begin{code}
zipWithVec  ::  (Syntactic a, Syntactic b) =>
                (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithVec f (Vec m g) (Vec n h)  =   Vec (minim m n) (\i -> f (g i) (h i))

sumVec                            ::  (Syntactic a, Num a) => Vector a -> a
sumVec (Vec n g)                  =   for n 0 (\i x -> x + g i)
\end{code}
Computing |zipWithVec f u v| combines vectors |u| and |v| pointwise with |f|,
and computing |sumVec v| sums the elements of vector |v|.

We can easily define any function over vectors where each vector
element is computed independently, including |drop|, |take|,
|reverse|, vector concatentation, and the like, but it may be harder
to do so when there are dependencies between elements, as in computing
a running sum.

\subsection{Fusion}

Using our primitives, it is easy to compute the scalar product of two vectors.
\begin{code}
scalarProd      ::  (Syntactic a, Num a) => Vector a -> Vector a -> a
scalarProd u v  =   sumVec (zipWithVec (*) u v)
\end{code}
An important consequence of the style of definition we have adopted is
that it provides lightweight fusion. The above definition would not produce
good C code if it first computed |zipWith (*) u v|, put the result into an
intermediate vector |w|, and then computed |sumVec w|. Fortunately, it does
not. Assume |u| is |Vec m g| and |v| is |Vec n h|. Then we can simplify
|scalarProd u v| as follows.
\[
\begin{array}{cl}
         &  |scalarProd u v|  \\
\leadsto &  |sumVec (zipWith (*) u v)|  \\
\leadsto &  |sumVec (zipWith (*) (Vec m g) (Vec n h)|  \\
\leadsto &  |sumVec (Vec (min m n) (\i -> g i * h i)|  \\
\leadsto &  |for (min m n) (\i x -> x + g i * h i)|
\end{array}
\]
Indeed, we can see that by construction that whenever we combine two
primitives the intermediate vector is always eliminated, a stronger
guarantee than provided by conventional optimising compilers.

There are some situations where fusion is not beneficial, notably
when an intermediate vector is accessed many times fusion will cause
the elements to be recomputed.  An alternative is to materialise the
vector in memory with the following function.
\begin{code}
memorise            ::  Syntactic a => Vector a -> Vector a
memorise (Vec n g)  =   Vec n (\i -> fromDp (ArrIx (Arr n (toDp . g)) i))
\end{code}
The above definition depends on common subexpression elimination
or observable sharing to ensure |Arr n (toDp . g)| is computed
once, rather than once for each element of the resulting vector.

For example, if
\begin{code}
blur :: Syntactic a => Vector a -> Vector a
\end{code}
%if False
\begin{code}
blur = undefined
\end{code}
%endif
averages adjacent elements of a vector, then one may choose to
compute either
\begin{center}
|blur (blur v)| ~~~or~~~ |blur (memorise (blur v))|
\end{center}
with different trade-offs between recomputation and memory usage.
Strong guarantees for fusion in combination with |memorize| gives
the programmer a simple interface which provides powerful optimisation
combined with fine control over memory usage.

%if False
\begin{code}
idVec     ::  Dp Int -> Vector (Dp Int)
idVec n   =   Vec n id

ex0       ::  Dp Int
ex0       =   scalarProd (idVec 10) (fmap (* 2) (idVec 10))

check0    ::  Int
check0    =   sum (zipWith (*) [0..9] (map (*2) [0..9]))

main      ::  Bool
main      =   eval ex0 == check0
\end{code}
%endif


