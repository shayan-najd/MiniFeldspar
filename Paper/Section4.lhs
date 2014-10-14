%if False
\begin{code}
\end{code}
%endif

The CDSL technique is powerful. In a little over seven pages,
we explained sufficiently how to combinine deep and shallow
embedding to produce a library that allows all the CDSL code
in Section~\ref{sec:overview} to run.

Readers might reasonably expect another seven pages explaining the
comparable parts of the QDSL library.  But there is no comparable
part!  The combination of deep and shallow embedding in CDSL allows
code in the target language to appear almost, but not quite, identical
to code in the host language. In contrast, quotation trivially gives
us host and target (quoted) languages that are identical.

All that is required is to ensure that any standard prelude functions used
inside QDSL quotations are understood and translated by our QDSL
processor. In this case, the only such functions used are arithmetic
primitives, plus the |Maybe| type and the functions |return|, |(>>=)|,
and |maybe|. In particular, the QDSL processor needs to replace
invocations of these three functions by their definitions. The
normaliser described in the next section will then completely eliminate
all occurrences of the |Maybe| type from the target program.

For purposes of comparison, our CDSL and QDSL implementations both produce
abstract syntax trees for target code represented as terms of type
|Dp|. The postprocessor that converts |Dp| to C code is shared among
both implementations.