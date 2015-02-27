%format testQt    (x) (y) = "\fi" y
%format testNrmQt (x) (y) = "\fi" y
%format testNrmSmpQt (x) (y) = "\fi" y
%if False
\begin{code}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGe TypeSynonymInstances #-}

import Prelude hiding (Int)
import qualified Prelude as P

import Data.Array(Array)

import QFeldspar.QDSL hiding (div,while,for,qdsl,min)
import qualified QFeldspar.CDSL as CDSL
import qualified QFeldspar.QDSL as QDSL

type C = String

class (Type a , FO a) => Rep a

test :: Bool
test = ex1 && ex2 && ex3 && ex4 && ex5

appVec = append

uniVec = [|| \ s -> Vec 1  (\ _i -> s) ||]
\end{code}
%endif
%format P.Int = Int

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
\begin{code}
qdsl :: (Rep a , Rep b) => Qt (a -> b) -> C
\end{code}
%if False
\begin{code}
qdsl = QDSL.qdsl
\end{code}
%endif
Here |Qt a| represents a Haskell term of type |a|, its
\emph{quoted} representation, and type |C| represents code in C.
The top-level function expects a quoted term representing
a function from type |a| to type |b|, and returns C code
that represents this function.
% (a @main@ routine) ***Shayan***: it produces a C function, not a main routine

Not all types representable in Haskell are easily representable
in the target language, C. For instance, we do not wish our target
C code to manipulate higher-order functions.  The argument
type |a| and result type |b| of the main function must be representable,
which is indicated by the type-class restrictions |Rep a| and |Rep b|.
Representable types include integers, floats, and pairs where the components
are both representable.
\begin{code}
instance Rep Int
instance Rep Float
instance (Rep a, Rep b) => Rep (a,b)
\end{code}
It is easy to add triples and larger tuples.

\subsection{A first example}
\label{subsec:power}

Let's begin by considering the ``hello world'' of program generation,
the power function, raising a float to a given integer.
Since division by zero is undefined, we
arbitrarily choose that raising zero to a negative power yields zero.
Here is an optimised power function represented using QDSL:
\begin{code}
power :: P.Int -> Qt (Float -> Float)
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
indicated by |[||||...||||]| and unquoting by |$$(...)|.

Evaluating |power (-6)| yields the following:
\begin{code}
{-"\iffalse"-}
ex1 = testQt (power (-6)) ([||  \x ->  if x == 0 then 0 else
                                  1 / (\x ->  (\y -> y * y)
                                    ((\x -> (x * ((\x -> (\y -> y * y)
                                      ((\x -> (x * ((\x -> 1) x))) x)) x))) x)) x ||])
\end{code}
Normalising using the technique of Section~\ref{sec:subformula},
with variables renamed for readability, yields the following:
\begin{code}
{-"\iffalse"-}
ex2 = testNrmQt (power (-6)) ([|| \u ->  if u == 0 then 0 else
             let v = u * 1 in
             let w = u * (v * v) in
             1 / (w * w)  ||])
\end{code}
With the exception of the top-level term, all of the overhead of
lambda abstraction and function application has been removed; we
explain below why this is guaranteed by the subformula property.
From the normalised term it is easy to generate the final C code:
\begin{lstlisting}
float prog (float u) {
  float w; float v; float r;
  if (u == 0.0f) {
    r = 0.0f;
  } else {
    v = (u * 1.0f);
    w = (u * (v * v));
    r = (1.0f / (w * w));
  }
  return r;
}
\end{lstlisting}
By default, we always generate a routine called \texttt{prog};
it is easy to provide the name as an additional parameter if required.

%  float prog (float u) {
%    if (u == 0) {
%      return 0;
%    } else {
%      float v = u * 1;
%      float w = u * (v * v);
%      return 1 / (w * w);
%    }
%  }

% ***Shayan***:
%               after macro expansion, and alpha renaming variables,
%               we currently get the following:
% float func (float u)
% {
%   float w;
%   float v;
%   float r;
%   if ((u == 0.0f))
%   {
%     r = 0.0f;
%   }
%   else
%   {
%     v = (u * 1.0f);
%     w = (u * (v * v));
%     r = (1.0f / (w * w));
%   }
%   return r;
% }

Depending on your point of view, quotation in this form of QDSL is
either desirable, because it makes manifest the staging, or
undesirable because it is too noisy.  QDSL enables us to ``steal'' the
entire syntax of the host language for our DSL.  In Haskell, an EDSL can
use the same syntax for arithmetic operators, but must use a different
syntax for equality tests and conditionals, as explained in
Section~\ref{sec:qdsl-vs-edsl}.

Within the quotation brackets there appear lambda abstractions and
function applications, while our intention is to generate first-order
code. How can the QDSL~Feldspar user be certain that such function
applications do not render transformation to first-order code
impossible or introduce additional runtime overhead?
The answer is the subformula property.
\\
\todo{Shayan}{Suggest rephrasing}
% \shayan{Above sentence should be rephrased. It is too long,
%        and hard to read.}


\subsection{The subformula property}
\label{subsec:subformula}

Gentzen's subformula property guarantees that any proof can be
normalised so that the only formulas that appear within it are
subformulas of one of the hypotheses or of the conclusion of the
proof.  Viewed through the lens of Propositions as Types
\citep{Howard-1980,Wadler-2015}, also known as the Curry-Howard
isomorphism, Gentzen's subformula property guarantees that any term
can be normalised so that the type of each of its subterms is a
subformula of either the type of one of its free variables (corresponding
to hypotheses) or the term itself (corresponding to the conclusion).
Here the subformulas of a type are the type itself and the subformulas of
its parts, where the parts of |a -> b| are |a| and |b|, the parts of
|(a,b)| are |a| and |b|, and that types |Int| and |Float| have no
parts.
See Proposition~\ref{prop:subformula} in Section~\ref{sec:subformula}.)

% the only part of |Arr a| is |a|

Further, it is easy to sharpen Gentzen's proof to guarantee a
sharpened subformula property: any term can be normalised so that the
type of each of its proper subterms is a proper subtype of either the
type of one of its free variables (corresponding to hypotheses) or the
term itself (corresponding to the conclusion).  Here the proper
subterms of a term are all subterms save for free variables and the
term itself, and the proper subformulas of a type are all subformulas
save for the type itself.  In the example of the previous subsection,
the sharpened subformula property guarantees that after normalisation
a closed term of type |float -> float| will only have proper subterms
of type |float|, which is indeed true for the normalised term.
(See Proposition~\ref{prop:sharpened} in Section~\ref{sec:subformula}.)

The subformula property depends on normalisation of terms, but
complete normalisation is not always possible or desirable.  The
extent of normalisation may be controlled by introducing uninterpreted
constants.  In particular, we introduce the uninterpreted constant
\begin{spec}
save :: Rep a => a -> a
\end{spec}
of arity $1$, which is equivalent to the identity function
on representable types; a use of |save| appears in
Section~\ref{subsec:arrays}.
In a context with recursion, we take
\begin{spec}
fix :: (a -> a) -> a
\end{spec}
as an uninterpreted constant.

% In a context where we wish to avoid
% unfolding an application |L M|, where |L| and |M| are terms,
% we take |id :: a -> a| as an uninterpreted constant,
% and replace |L M| by |id L M|.
% In a context with recursion, we take |fix :: (a -> a) -> a|
% as an uninterpreted constant.

(As a technical convenience, we also require constants of a given arity
to be fully applied, and treat subterms and subformulas of these specially.
See Section~\ref{sec:subformula}.)

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
power' :: P.Int -> Qt (Float -> Maybe Float)
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

power'' ::  P.Int -> Qt (Float -> Float)
power'' n =
  [|| \ x ->  maybe 0 (\y -> y) ($$(power' n) x)||]
\end{code}

%if False
\begin{code}
ex3 = testNrmSmpQt (power (-6)) (power'' (-6))
ex4 = qdsl (power (-6)) == qdsl (power'' (-6))
\end{code}
%endif
Here |sqr| is as before. Evaluation and normalisation of
|power (-6)| and |power'' (-6)| yield identical terms
(up to renaming), and hence applying |qdsl| to these yields
identical C code.

The subformula property is key: because the final type of the result
does not involve |Maybe| it is certain that normalisation will remove
all its occurrences.  Occurrences of |do| notation are expanded to
applications of |(>>=)|, as usual.  Rather than taking |return|,
|(>>=)|, and |maybe| as free variables (whose types have subformulas
involving |Maybe|), we treat them as known definitions to be
eliminated by the normaliser.  The |Maybe| type is a sum type, and
normalisation for sums is as described in
Section~\ref{sec:subformula}.

% We have chosen not to make |Maybe| a representable type, which
% prohibits its use as argument or result of the top-level function
% passed to |qdsl|. An alternative choice is possible, as we will see
% when we consider arrays, in Section~\ref{subsec:arrays} below.

\subsection{While}
\label{subsec:while}

Code that is intended to compile to a @while@ loop in C is indicated
in QDSL~Feldspar by application of |while|.
\begin{code}
while :: (Rep s) => (s -> Bool) -> (s -> s) -> s -> s
\end{code}
%if False
\begin{code}
while  = QDSL.while
\end{code}
%endif
Rather than using side-effects, |while| takes three
arguments: a predicate over the current state, of type |s -> Bool|; a
function from current state to new state, of type |s -> s|; and an
initial state of type |s|; and it returns a final state of type |s|.
So that we may compile |while| loops to C, the type of the state
is constrained to representable types.

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
of the |for| loop returned. Here |while|, like |snd| and |(+)|,
is a constant known to QDSL Feldspar, and so not enclosed in |$$| antiquotes.

As an example, we can define Fibonacci using a |for| loop.
\begin{code}
fib :: Qt (Int -> Int)
fib =  [|| \n -> fst ($$for n (0,1) (\i (a,b) -> (b,a+b))) ||]
\end{code}

Again, the subformula property plays a key role.
As explained in Section~\ref{subsec:subformula}, primitives of the
language to be compiled, such as |(*)| and |while|, are treated as
free variables or constants of a given arity.
As described in Section~\ref{sec:subformula},
we can ensure that after normalisation every occurence of |while|
has the form
\begin{spec}
while (\s -> ...) (\s -> ...) (...)
\end{spec}
where the first ellipses has type |Bool|,
and both occurrences of the bound variable |s|
and the second and third ellipses all have the same type,
that of the state of the while loop.

Unsurprisingly, and in accord with the subformula property, each
occurrence of |while| in the normalised code will contain subterms
with the type of its state. The restriction of state to representable
types increases the utility of the subformula property. For instance,
since we have chosen that |Maybe| is not a representable type, we can
ensure that any top-level function without |Maybe| in its type will
normalise to code not containing |Maybe| in the type of any subterm.
In particular, |Maybe| cannot appear in the state of a |while| loop,
which is restricted to representable types.
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
\begin{code}
type Arr a  =  Array Int a
\end{code}
\begin{spec}
data Vec a  =  Vec Int (Int -> a)
\end{spec}
Values of type |Arr| are representable, assuming that the
element type is representable, while values of type |Vec|
are not representable.
\begin{code}
instance (Rep a) => Rep (Arr a)
\end{code}

For arrays, we assume the following primitive operations.
\begin{spec}
mkArr   ::  (Rep a) => Int -> (Int -> a) -> Arr a
lnArr   ::  (Rep a) => Arr a -> Int
ixArr   ::  (Rep a) => Arr a -> Int -> a
\end{spec}
The first populates a manifest array of the given
size using the given indexing function, the second
returns the length of the array, and the third returns
the array element at the given index.
Array components must be representable.

We define functions to convert between the two representations in the
obvious way.
\begin{code}
toVec        ::  Rep a => Qt (Arr a -> Vec a)
toVec        =   [|| \a -> Vec (lnArr a) (\i -> ixArr a i) ||]

fromVec      ::  Rep a => Qt (Vec a -> Arr a)
fromVec      =   [|| \(Vec n g) -> mkArr n g ||]
\end{code}

It is straightforward to define operations on vectors,
including combining corresponding elements of two vectors,
summing the elements of a vector, dot product of two vectors,
and norm of a vector.
When combining two vectors, the length of the result is the
minimum of the lengths of the arguments.
\begin{code}
minim    ::  Ord a => Qt (a -> a -> a)
minim    =   [|| \x y -> if x < y then x else y ||]

zipVec   ::  Qt ((a -> b -> c) -> Vec a -> Vec b -> Vec c)
zipVec   =   [||  \f (Vec m g) (Vec n h) ->
                        Vec ($$minim m n) (\i -> f (g i) (h i)) ||]

sumVec   ::  (Rep a, Num a) => Qt (Vec a -> a)
sumVec   =   [|| \(Vec n g) -> $$for n 0 (\i x -> x + g i) ||]

dotVec   ::  (Rep a, Num a) => Qt (Vec a -> Vec a -> a)
dotVec   =   [|| \u v -> $$sumVec ($$zipVec (*) u v) ||]

normVec  ::  Qt (Vec Float -> Float)
normVec  =   [|| \v -> sqrt ($$dotVec v v) ||]
\end{code}
The third of these uses the |for| loop defined in
Section~\ref{subsec:while}.

Our final function cannot accept |Vec| as input, since
the |Vec| type is not representable, but it can accept
|Arr| as input.  For instance, if we invoke |qdsl| on
\[
|[|||| $$normVec . $$toVec ||||]|
\]
the quoted term normalises to
\begin{code}
{-"\iffalse"-}
ex5 = testNrmSmpQt ([|| \ v -> $$normVec ($$toVec v) ||]) [|| (\a -> sqrt (snd (while (\ s -> fst s < lnArr a)
                                     (\ s -> let i = fst s in
                                             (i + 1 , snd s + (ixArr a i * ixArr a i)))
                                     (0 , 0.0)))) ||]
\end{code}
from which it is easy to generate C code.

The vector representation makes it easy to define any
function where each vector element is computed independently,
such as the examples above,
vector append (|appVec|)
and creating a vector of one element (|uniVec|),
but is less well suited to functions with dependencies
between elements, such as computing a running sum.

Types and the subformula property help us to guarantee fusion.  The
subformula property guarantees that all occurrences of |Vec| must be
eliminated, while occurrences of |Arr| will remain. There are some
situations where fusion is not beneficial, notably when an
intermediate vector is accessed many times, in which case fusion will
cause the elements to be recomputed. An alternative is to materialise
the vector as an array with the following function.
\begin{code}
memorise  ::  Rep a => Qt (Vec a -> Vec a)
memorise = [|| $$toVec . save . $$fromVec ||]
\end{code}
Here we interpose |save|, as defined in Section~\ref{subsec:subformula}
to forestall the fusion that would otherwise occur. For example, if
\begin{code}
blur :: Qt (Vec Float -> Vec Float)
blur = [|| \a -> $$zipVec  (\x y -> sqrt (x*y))
                           ($$appVec ($$uniVec 0) a)
                           ($$appVec a ($$uniVec 0)) ||]
\end{code}
computes the geometric mean of adjacent elements of a vector, then one may choose to
compute either
\begin{center}
|[||||$$blur . $$blur||||]| ~~~or~~~ |[||||$$blur . $$memorise . $$blur||||]|
\end{center}
with different trade-offs between recomputation and memory usage.
Strong guarantees for fusion in combination with |memorise| give the
programmer a simple interface which provides powerful optimisations
combined with fine control over memory usage.

We have described the application of the subformula to array
fusion as based on ``pull arrays'' \citep{svenningsson:combining},
but the same technique should also apply to other techniques that
support array fusion, such as ``push arrays'' \citep{claessen:push}.
