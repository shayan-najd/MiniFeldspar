
\subsection{First example}
\label{sec:first-example}

Let's begin by considering the ``hello world'' of program generation,
the power function. Since division by zero is undefined, we arbitrarily
choose that raising zero to a negative power yields zero.
\begin{code}
power :: Int -> Float -> Float
power n x =
  if n < 0 then
    if x == 0 then 0 else 1 / power (-n) x
  else if n == 0 then
    1
  else if even n then
    sqr (power (n `div` 2) x)
  else
    x * power (n-1) x

sqr    ::  Float -> Float
sqr x  =   x * x
\end{code}
Our goal is to generate code in the programming language~C.
For example,
\begin{lstlisting}
  float main (float u) {
    if (u == 0) {
      return 0;
    } else {
      float v = u * 1;
      float w = u * (v * v);
      return 1 / (w * w);
    }
  }
\end{lstlisting}
should result from instantiating |power| to |(-6)|.

For CDSL, we assume a type |Dp a| to represent a term
of type |a|, and function
\begin{code}
edslC :: (Dp a -> Dp b) -> C
\end{code}
to generate a \texttt{main} function corresponding to its
argument, where |C| is a type that represents |C| code.
Here is a solution to our problem using Dp.
\begin{code}
power :: Int -> Dp Float -> Dp Float
power n x =
  if n < 0 then
    x .==. 0 ? (0,  1 / power (-n) x)
  else if n == 0 then
    1
  else if even n then
    sqr (power (n `div` 2) x)
  else
    x * power (n-1) x

sqr    ::  Dp Float -> Dp Float
sqr y  =   y * y
\end{code}
Invoking |edslC (power (-6))| generates the C code above.

Type |Float -> Float| in the original becomes
\[
|Dp Float -> Dp Float|
\]
in the CDSL solution, meaning that |power n| accepts a representation
of the argument and returns a representation of that argument raised
to the $n$'th power.

In CDSL, the body of the code remains almost---but not
quite!---identical to the original.  Clever encoding tricks, which we
will explain later, permit declarations, function calls, arithmetic
operations, and numbers to appear the same whether they are to be
executed at generation-time or run-time.  However, for reasons that we
will explain later, comparison and conditionals appear differently
depending on whether they are to be executed at generation-time or
run-time, using |M == N| and |if L then M else N| for the former but
|M .==. N| and |L ?  (M, N)| for the latter.

Assuming |x| contains a value of type |Dp Float| denoting an object
variable |u| of type float, evaluating |power (-6) x| yields following.
\begin{code}
(u .==. 0) ? (0, 1 / (u * ((u * 1) * (u * 1))) * (u * ((u * 1) * (u * 1))))
\end{code}
Applying common-subexpression elimination, or using a technique such
as observable sharing, permits recovering the sharing structure.
\[
\begin{array}{r@@{~}||@@{~}l}
|v| & |(u * 1)|  \\
|w| & |u * (v * v)| \\
|main| &  |(u .==. 0) ? (0, 1/(w*w))|
\end{array}
\]
It is easy to generate the final C code from this structure.

By contrast, for QDSL, we assume a type |Qt a| to represent a
quoted term of type |a|, and function
\begin{code}
qdslC :: Qt (a -> b) -> C
\end{code}
to generate a \texttt{main} function corresponding to its
argument.  Here is a solution to our problem using QDSL.
\begin{code}
power :: Int -> Exp (Float -> Float)
power n =
  if n < 0 then
    <|| \x -> if x == 0 then 0 else 1 / $(power (-n)) x ||>
  else if n == 0 then
    <|| \x -> 1 ||>
  else if even n then
    <|| \x -> $(sqr) ($(power (n `div` 2)) x) ||>
  else
    <|| \x -> x * $(power (n-1)) x ||>

sqr  ::  Exp (Float -> Float)
sqr  =   <|| \y -> y * y ||>
\end{code}
Invoking |qdslC (power (-6))| generates the C code above.

Type |Float -> Float| in the original becomes
\[
|Qt (Float -> Float)|
\]
in the QDSL solution, meaning that |power n| returns a quotation
of a function that accepts an argument and returns that
argument raised to the $n$'th power.

In QDSL, the body of the code changes more substantially. The typed
quasi-quoting mechanism of Template Haskell is used to indicate which
code executes at which time.  Unquoted code executes at
generation-time while quoted code executes at run-time. Quoting is
indicated by \( <|| \cdots ||> \) and unquoting by \( \$(\cdots) \).
Here, by the mechanism of quoting, without any need for tricks,
the syntax for code excecuted at both generation-time and run-time is
identical for all constructs, including comparison and conditionals.

Now evaluating |power (-6)| yields the following.
\begin{code}
<|| \x ->  if x == 0 then 0 else
             1 / (\y -> y * y) (x * (\y -> y * y) (x * 1)) ||>
\end{code}
Normalising the term, with variables renamed
for readability, yields the following.
\begin{code}
<|| \u ->  if u == 0 then 0 else
             let v = u * 1 in
             let w = u * (v * v) in
             1 / w * w  ||>
\end{code}
It is easy to generate the final C code from
the normalised term.

Here are some points of comparison between the two approaches.
\begin{itemize}

\item CDSL requires some term forms, such as comparison and
conditionals, to differ between the host and embedded languages.
In contrast, QDSL enables the host and embedded languages to
appear identical.

\item CDSL permits the host and embedded languages to intermingle
seamlessly. In contrast, QDSL requires syntax to separate quoted and
unquoted terms, which (depending on your point of view) may be
considered as an unnessary distraction or as drawing a useful
distinction between generation-time and run-time.  If one takes the
former view, the type-based approach to quotation found in C\# and
Scala might be preferred.

\item CDSL typically develops custom shallow and deep embeddings for
each application, although these may follow a fairly standard pattern
(as we review in Section~\ref{sec:shallow-deep}).  In contrast, QDSL
may share the same representation for quoted terms across a range of
applications; the quoted language is the host language, and does not
vary with the specific domain.

\item CDSL loses sharing, which must later be recovered be either
common subexpression elimination or applying a technique such as
observable sharing.  Common subexpression elimination can be
expensive, as we will see in the FFT example in Section~\ref{sec:fft}.
Observable sharing is less costly, but requires stepping outside a
pure functional model. In contrast, QDSL preserves sharing throughout.

\item CDSL yields the term in normalised form in this case, though
there are other situations where a normaliser is required (see
Section~\ref{sec:second-example}).  In contrast, QDSL yields an unwieldy term
that requires normalisation.  However, just as a single representation
of QDSL terms suffices across many applications, so does a single
normaliser---it can be built once and reused many times.

\item Once the deep embedding or the normalised quoted term is
produced, generating the domain-specific code is similar for
both approaches.

\end{itemize}

\subsection{Second example}
\label{sec:second-example}

In the previous code, we arbitrarily chose that raising zero to a
negative power yields zero. Say that we wish to exploit the |Maybe| type
to refactor the code, separating identifying the exceptional case
(negative exponent of zero) from choosing a value for this case (zero).
We decompose |power| into two functions |power'| and |power''|, where the first
returns |Nothing| in the exceptional case, and the second maps |Nothing|
to a suitable default value.
\begin{code}
power' ::  Int -> Float -> Maybe Float
power' n x =
  if n < 0 then
    if x == 0 then Nothing else do y <- power' (-n) x; return (1 / y)
  else if n == 0 then
    return 1
  else if even n then
    do y <- power' (n `div` 2) x; return (sqr y)
  else
    do y <- power' (n-1) x; return (x * y)

power''      ::  Int -> Float -> Float
power'' n x  =   maybe 0 (\x -> x) (power' n x)
\end{code}
Here |sqr| is as before. The above uses
\begin{code}
data Maybe a = Nothing | Just a
return  ::  a -> Maybe a
(>>=)   ::  Maybe a -> (a -> Maybe b) -> Maybe b
maybe   ::  b -> (a -> b) -> Maybe a -> b
\end{code}
from the Haskell prelude. Type |Maybe| is declared as a monad, enabling the |do| notation,
which translates into |(>>=)|.
The same C code as before should result from instantiating |power''| to |(-6)|.

(In this case, the refactored function is arguably clumsier than the original,
but clearly it is desirable to support this form of refactoring in general.)

In CDSL, type |Maybe| is represented by type |Option|.
Here is the refactored code.
\begin{code}
power' :: Int -> Dp Float -> Option (Dp Float)
power' n x  =
  if n < 0 then
    (x .==. 0) ? (none, do y <- power' (-n) x; return (1 / y))
  else if n == 0 then
    return 1
  else if even n then
    do y <- power' (n `div` 2) x; return (sqr y)
  else
    do y <- power' (n-1) x; return (x*y)

power''      ::  Int -> Dp Float -> Dp Float
power'' n x  =   option 0 (\y -> y) (power' n x)
\end{code}
Here |sqr| is as before. The above uses the functions
\begin{code}
none   	::  Option a
return 	::  a -> Option a
(>>=)  	::  Option a -> (a -> Option b) -> Option b
option 	::  (Syntactic a, Syntactic b) => b -> (a -> b) -> Option a -> b
\end{code}
from the CDSL library. Details of the type |Option| and the type class |Syntactic|
are explained in Section~\ref{sec:option}.
Type |Option| is declared as a monad, enabling the |do| notation,
which translates into |(>>=)|.
Invoking |edslC (power'' (-6))| generates the same C code as
the previous example.

In order to be easily represented in C, type |Option a| is represented as a pair
consisting of a boolean and the representation of the type |a|; in the case that
corresponds to |Nothing|, the boolean is false and a default value of type |a|
is provided.  The CDSL term generated by evaluating |power (-6) 0| is large and
unscrutable:
\begin{code}
(((fst ((x == (0.0)) ? ((((False) ? ((True), (False))), ((False) ?  (undef,
undef))), ((True), ((1.0) / ((x * ((x * (1.0)) * (x * (1.0)))) * (x * ((x *
(1.0)) * (x * (1.0)))))))))) ? ((True), (False))) ?  (((fst ((x == (0.0)) ?
((((False) ? ((True), (False))), ((False) ?  (undef, undef))), ((True), ((1.0) /
((x * ((x * (1.0)) * (x * (1.0)))) * (x * ((x * (1.0)) * (x * (1.0)))))))))) ?
((snd ((x == (0.0)) ?  ((((False) ? ((True), (False))), ((False) ? (undef,
undef))), ((True), ((1.0) / ((x * ((x * (1.0)) * (x * (1.0)))) * (x * ((x *
(1.0)) * (x * (1.0)))))))))), undef)), (0.0)))
\end{code}
(Details of why this is the term generated will become clearer after
Section~\ref{sec:option}.)  Before, evaluating |power| yielded an CDSL term
essentially in normal form, save for the need to use common subexpression
elimination or observable sharing to recover shared structure.  However, this is
not the case here. Rewrite rules including the following need to be repeatedly
applied.

\todo{Fix typesetting of |M[L:=P]|.}
\begin{eqnarray*}
|fst (M, N)| &\leadsto& M \\
|snd (M, N)| &\leadsto& N \\
|fst (L ? (M, N))| &\leadsto& |L ? (fst M, fst N)| \\
|snd (L ? (M, N))| &\leadsto& |L ? (snd M, snd N)| \\
|True ? (M,N)| &\leadsto& |M| \\
|False ? (M,N)| &\leadsto& |N| \\
|(L ? (M, N)) ? (P,Q)| &\leadsto& |L ? (M ? (P,Q)) ? (N ? (P,Q))| \\
|L ? (M, N)| &\leadsto& |L ? (M[L:=True], N[L:=False])|
\end{eqnarray*}
Here |L|,|M|,|N|,|P|,|Q| range over |Dp| terms, and
|M[L:=P]| stands for |M| with each occurence of |L| replaced by |P|.
After applying these rules, common subexpression
elimination yields the same structure as in the previous subsection,
from which the same C code is generated.

Hence, an advantages of the CDSL approach---that it generates
terms essentially in normal form---turns out to be restricted
to a limited set of types, including functions and products,
but excluding sums. If one wishes to deal with sum types,
separate normalisation is required. This is one reason
why we do not consider normalisation as required by QDSL
to be particularly onerous.

In QDSL, the type |Maybe| can be represented by its
run-time equivalent, which we also call |Maybe|.
Here is the refactored code.
\begin{code}
power' :: Int -> Qt (Float -> Maybe Float)
power' n =
  if n < 0 then
    <|| \x ->  if x == 0 then Nothing else
                 do y <- $(power' (-n) x); return (1 / y) ||>
  else if n == 0 then
    <|| \x -> return 1 ||>
  else if even n then
    <|| \x -> do y <- $(power' (n `div` 2) x); return $(sqr y) ||>
  else
    <|| \x -> do y <- $(power' (n-1) x); return (x * y) ||>

power''      ::  Int -> Qt (Float -> Float)
power'' n x  =   <|| maybe 0 (\x -> x) $(power' n x) ||>
\end{code}
Here |sqr| is as before,
and |Nothing|, |return|, |(>>=)|, and |maybe| are typed as before,
and provided for use in quoted terms by the QDSL library.

Evaluating |Qt (powerQ'' (-6))| yields a term of similar complexity
to the term yielded by the CDSL. Normalisation by the rules discussed
in Section~\ref{sec:normalise} reduces the term to the same form
as before, which in turn generates the same C as before.

Here are some further points of comparison between the two approaches.
\begin{itemize}

\item Both CDSL and QDSL can exploit notational conveniences in the
host language. The example here exploits Haskell |do| notation; the
embedding SQL in F\# by \citet{CheneyLW13} expoited F\# sequence
notation. For the CDSL, exploiting |do| notation just requires
instantiating |return| and |(>>=)| correctly. For the QDSL, it is
also necessary for the normaliser to recognise and expand
|do| notation and to substitute appropriate instances of
|return| and |(>>=)|.

\item As this example shows, sometimes both CDSLs and QDSLs
may require normalisation. Each CDSL usually has a distinct
deep representation and so requires a distinct normaliser.
In contrast, all QDSLs can share the representation of the
quoted host language, and so can share a normaliser.

\end{itemize}
