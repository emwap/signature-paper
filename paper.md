---
documentclass: llncs
title: Programmable Signatures
author:
  - Anders Persson
  - Emil Axelsson
abstract: |
  When compiling EDSLs into other languages, the compiler translates types in the source language into corresponding types in the target language.
  The translation is often driven by a small set of rules that map a single type in the source language into a single type in the target language.
  This simple approach is limiting when there are multiple possible mappings, and it may lead to poor interoperability and performance in the generated code.

  Instead of hard-wiring a single set of translation rules into a compiler, this paper introduces a small language that lets the programmer describe the mapping of each argument and function separately.

numbersections: true
bibliography: paper.bib
csl: springer-lecture-notes-in-computer-science.csl
link-citations: true
filter: pandoc-citeproc
header-includes: |
  \institute{Chalmers University of Technology}
  \input{listings_haskell.tex}
  \usepackage{cleveref}
---


\newcommand{\todo}[1]{\marginpar{\scriptsize\textcolor{red}{TODO~{#1}}}}


``` {.haskell .hide}
import Feldspar (Data,Word32)
import qualified Feldspar as F
import qualified Feldspar.Vector as F
import qualified Feldspar.Algorithm.CRC as F
import Feldspar.Compiler.Signature
```


# Introduction

  <!--
- Compiling from a typed embedded language
- Fixed mapping from host language type to target language type
- A fixed mapping is too restrictive
  -->

[^FeldsparLanguageHackage]: <https://hackage.haskell.org/package/feldspar-language>
[^FeldsparCompilerHackage]: <https://hackage.haskell.org/package/feldspar-compiler>

Feldspar is an embedded domain specific language written in Haskell [@axelsson2010feldspar; @axelsson2010design].[^FeldsparLanguageHackage]
The purpose of Feldspar is to implement high-performance software, especially in the domain of signal processing in embedded systems [@persson2014towards].

Feldspar comes with an optimizing compiler that translates Feldspar expressions into C99 code.[^FeldsparCompilerHackage]
When translating a function signature, the compiler uses a specific calling convention as detailed in [@persson2014towards, ch. 1.4.2]:

- All functions return `void`{.C}
- Scalar values are passed by value
- Structured values (structs, arrays) are passed by reference
- Arrays are represented using a data structure `struct array`{.C}
- Return values are passed through caller provided pointers

For example, the following is the type of an FFT function in Feldspar:

``` {.haskell}
fft :: Data [Double] -> Data [Double]
```
``` {.haskell .hide}
fft = id
```
The type constructor `Data` denotes a Feldspar expression, and its parameter denotes the type of value computed by that expression. For historical reasons, Feldspar uses `[]` to denote immutable arrays.

The compiler translates the `fft` function into the following C99 signature,
``` {.ghci}
cgenProto $ lam $ \xs -> ptr "fft" $ fft xs
```
where `struct array`{.C} is a Feldspar specific data structure with metadata, such as the number of elements, and a pointer to the data area.

The Feldspar compiler uses its calling convention for a number of reasons, but the primary reasons are consistency and generality. The convention ensures that all arguments fit into a register, which helps avoid spilling arguments to the call stack. By passing arrays as references bundled with their length, the compiler can generate code that works with different array sizes and still preserve the same number of arguments.

A hard-wired set of mapping rules can be restrictive and introduce performance penalties. Code generated from Feldspar will be part of a larger system, and the calling convention is naturally dictated by the system rather than by the Feldspar compiler.



## Issues with Fixed Mappings

With a fixed signature mapping it is easy to derive the target language type from the source language type. But the fixed mapping leaves little room to change the generated signature to fit into existing software. Instead, separate wrapper functions have to be written and maintained.

In a typical embedded system, arrays are passed as two arguments: a pointer to the data buffer and an integer that gives the number of elements of the array. However, there are many variations on this theme. Should the length come before or after the buffer? Can the length argument be used for more than one array if they always have the same length? And so on.

Even if we allow flags to customize the compiler, a fixed set of mapping rules will never be able to cover all possible situations. Instead, we would like to put the exported signature in the hands of the programmer.

  <!--
Many compilers provide options to change the interpretation of program elements.
However, these options take effect for the entire invocation of the compiler, meaning that it is not possible to apply options only to some elements.
  -->

As a concrete example, take the following function for computing the scalar product of two vectors:

``` {.haskell}
scProd :: Data [Double] -> Data [Double] -> Data Double
```
``` {.haskell .hide}
scProd as bs = F.desugar $ F.scalarProd (F.thawPull1 as) (F.thawPull1 bs)
```
The generated signature with the default mapping is:

``` {.ghci}
cgenProto $ lam $ \as -> lam $ \bs -> ptr "scProd" $ scProd as bs
```
By default, the Feldspar compiler automatically makes up names for the arguments.
Apart from the problem that Feldspar's `struct array` is an unconventional array representation, this code may also be considered too general: it has to cater for the fact that the arrays may have different lengths. Since it does not make sense to call `scProd` with arrays of different lengths, a more appropriate signature might be:

``` {.C}
void scProd(double* v0, double* v1, int length, double* out);
```
\todo{We cannot really produce this with `native` since we need the length before it is captured}
Here, the arrays are passed as two pointers to the corresponding data buffers and a single length argument. This signature is more likely to occur in a practical system, and it has the advantage that the function does not have to decide what to do if the lengths are different. However, the system may expect a different order of the arguments, and might expect the result to be passed by value instead of by reference.

In addition to being able to customize the calling convention, we might also want to affect non-functional aspects of functions.

\todo{Variable names, annotations: `inline`, `UNPACK`, `restrict`, `volatile`, etc.}



## Contributions

To address the problems above, this paper presents two contributions:

- We define a simple EDSL to specify type conversions and annotations when exporting a Feldspar function to an external system (\cref{the-signature-language}).
- We give an implementation of the EDSL as a small wrapper around the existing Feldspar compiler (\cref{implementation}). The implementation relies on a simple interface to the underlying compiler, and the technique can easily be ported to other EDSLs for which the compiler implements the same interface.



# The Signature Language

Dissatisfied with hard-wired rules and global compiler options, we propose a small language as a more flexible way to drive the compiler.

The Signature language allows the programmer to express the mapping of individual arguments separately.
Specifically allows the programmer  to add annotations to every argument.
These annotations can be as simple as just giving a name to a parameter, using the `name` combinator.
Or, it can change the arity of the function by introducing new parameters, like the `exposeLength` combinator does.

- specify how the compiler should treat each argument, and result.
    - naming arguments, for readability and debugging.
    - control data representation from a performance perspective.
    - potentially generate interface code to bridge different representation formats.
    - the signature code does not become a wrapper around the original function, instead it is fused with the function body
    - since the signature is added before optimization, it can possibly enable more optimizations.

Like the Feldspar language, the Signature language is a typed embedded domain specific language, embedded in Haskell.
The Signature language preserves the type safety of the Felspar language.

The basic combinators `lam`, `res` and `ptr`, are used for argument positions and the result respectively.

``` {.haskell .skip #lst:signature-shallow style=float caption="Signature language (shallow embedding)"}
-- | Capture an argument
lam :: (Type a) => (Data a -> Signature b) -> Signature (a -> b)

-- | Capture and name an argument
name :: (Type a) => String -> (Data a -> Signature b) -> Signature (a -> b)

-- | Create a named function return either by value or reference
ret :: (Type a) => String -> Data a -> Signature a
ptr :: (Type a) => String -> Data a -> Signature a
```


As our running example, we will reuse the `scProd`{.haskell} function from \cref{issues-with-fixed-mappings}.

``` {.haskell .skip}
scProd :: Data [Double] -> Data [Double] -> Data Double
```
We can mimic the standard rules of the Feldspar compiler by wrapping the function in our combinators.

``` {.haskell}
ex1 = lam $ \xs -> lam $ \ys -> ptr "scProd" (scProd xs ys)
```
which generates the following C signature when compiled
``` {.ghci}
cgenProto ex1
```

We change the embedding to name the first argument
``` {.haskell}
ex2 = name "xs" $ \xs -> lam $ \ys -> ptr "scProd" (scProd xs ys)
```
resulting in
``` {.ghci}
cgenProto ex2
```

Finally, we change the function to return by value
``` {.haskell}
ex3 = name "xs" $ \xs -> name "ys" $ \ys -> ret "scProd" (scProd xs ys)
```
which produces

``` {.ghci}
cgenProto ex3
```


# Implementation

\todo{Describe the C-monad and CompExp}

The language is implemented as a combination of a shallow and a deep embedding.
The shallow embedding (\cref{lst:signature-shallow}), which is also the programmer interface, provides combinators to describe the mapping of a function.
The deep embedding (\cref{lst:signature-deep}) is interpreted by the compiler to apply the rules.

By using two separate embeddings it is possible to have a small set of constructs that the compiler has to deal with, while at the same time provide a rich set of combinators to the end user. This way of combining deep and shallow embeddings has been shown to be very powerful for implementing EDSLs [@svenningsson2013combining].

``` {.haskell .skip #lst:signature-deep style=float caption="Signature Language (deep embedding)"}
-- | Annotations to place on arguments or result
data Ann a where
  Empty  :: Ann a
  Native :: Type a => Data F.Length -> Ann [a]
  Named  :: String -> Ann a

-- | Annotation carrying signature description
data Signature a where
  Ret    :: (Type a) => String -> Data a -> Signature a
  Ptr    :: (Type a) => String -> Data a -> Signature a
  Lam    :: (Type a)
         => Ann a -> (Data a -> Signature b) -> Signature (a -> b)
```

We can think of `Signature` as adding top-level lambda abstraction and result annotations to the existing expression language `Data`. The use of a host-language function in the `Lam` constructor is commonly known as *higher-order abstract syntax* (HOAS) [@pfenning1988higher]. HOAS allows us to construct signatures without the need to generate fresh variable names. As we will see soon, names are instead generated when we generate code from the signature.

The signature is compiled by recursively traversing the `Lam` constructors and building up the argument list.
Finally, the `Res` node is compiled and combined with the arguments to produce the function signature.
The compilation of the function body is delegated to the Feldspar compiler.

\todo{The final paper will show in more detail how the signature is compiled into C code.}

``` {.haskell .skip #lst:translate-sig caption="Signature translation" style=float}
-- | Compile a @Signature@ to C code
translateFunction :: forall m a. (MonadC m) => Signature a -> m ()
translateFunction sig = go sig (return ())
  where
    go :: forall d. Signature d -> m () -> m ()
    go (Ret n a) prelude = do
      t <- compType a
      inFunctionTy t n $ do
        prelude
        e <- compExp a
        addStm [cstm| return $e; |]
    go (Ptr n a) prelude = do
      t <- compType a
      inFunction n $ do
        prelude
        e <- compExp a
        addParam [cparam| $ty:t *out |]
        addStm [cstm| *out = $e; |]
    go fun@(Lam Empty f) prelude = do
      t <- compTypePP (Proxy :: Proxy Data) (argProxy fun)
      v <- varExp <$> freshId
      C.Var n _ <- compExp v
      go (f v) $ prelude >> addParam [cparam| $ty:t $id:n |]
    go fun@(Lam n@(Native l) f) prelude = do
      t <- compTypePP (Proxy :: Proxy Data) (elemProxy n fun)
      w <- varExp <$> freshId
      C.Var m _ <- compExp w
      let n = appendId m "_buf"
      go (f w) $ do
        prelude
        len <- compExp l
        addLocal [cdecl| struct array $id:m = { .buffer = $id:n
                                              , .length=$len
                                              , .elemSize=sizeof($ty:t)
                                              , .bytes=sizeof($ty:t)*$len
                                              }; |]
        addParam [cparam| $ty:t * $id:n |]
    go fun@(Lam (Named s) f) prelude = do
      t <- compTypePP (Proxy :: Proxy Data) (argProxy fun)
      i <- freshId
      withAlias i s $ go (f $ varExp i) $ prelude >> addParam [cparam| $ty:t $id:s |]

    argProxy :: Signature (b -> c) -> Proxy b
    argProxy _ = Proxy

    elemProxy :: Ann [b] -> Signature ([b] -> c) -> Proxy b
    elemProxy _ _ = Proxy

    appendId :: C.Id -> String -> C.Id
    appendId (C.Id s loc) suf = C.Id (s++suf) loc
```

# Discussion and Future Work

Why is a new language needed?

- Why not just add annotations to the `Lam`{.haskell} constructor in Feldspar Core?
    - Signatures can be seen as an extension to the Core language.
    - Signatures can coexist with other similar extensions
    - Signatures have the same power (for top-level lambdas) as the Core `Lam`{.haskell} constructor
- Generialization of the Signature language is future work
- It is currently not possible to stack multiple annotations on the same argument
- Change the feldspar-compiler to use native arrays internally and make it possible to add other representations as signatures.

``` {.haskell}
sig = name "len" $ \len ->
      native len $ \as  ->
      native len $ \bs  ->
      ret "scProd" $ scProd as bs
```

``` {.ghci}
cgenDefinition sig
```

# Related Work

The final paper will consider techniques in other languages and systems, e.g. Scala Delite, the use of marks in model driven design and Matlab Coder.

MATLAB Coder [@matlab-coder][^MatlabCoder] is a tool that generates standalone C and C++ code from MATLAB code. One purpose of MATLAB Coder is to export MATLAB functions to an external system. Since MATLAB is dynamically typed, the same function can operate on values of different type. When generating C code, the user must specify a type for the function, and optionally sizes or size bounds for matrix arguments. This can be done on the command line using what can be seen as a restricted DSL.

However, judging from the officially available examples, the signature mapping of MATLAB Coder appears to be rather restricted. For example, stack allocated matrices are passed as two arguments: a pointer to a data buffer and a length vector. If a static size is given for the matrix, the length vector goes away. But if a different argument order is needed, or if one wants to use the same length vector for two different matrices, this likely requires introducing a wrapper function with a different interface.

  <!-- See http://se.mathworks.com/help/fixedpoint/ug/c-code-interface-for-unbounded-arrays-and-structure-fields.html -->

[^MatlabCoder]: Matlab Coder <http://www.mathworks.com/products/matlab-coder>

# References
