---
documentclass: llncs
title: Programmable Signatures
author:
  - Anders Persson
  - Emil Axelsson
abstract: |
  When compiling Embedded Domain Specific Languages (EDSLs) into other languages, the compiler translates types in the source language into corresponding types in the target language.
  The translation is often driven by a small set of rules that map a single type in the source language into a single type in the target language.
  This simple approach is limiting when there are multiple possible mappings, and it may lead to poor interoperability and poor performance in the generated code.
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
import Feldspar.Compiler.Signature hiding (Ann,Signature)
import qualified Feldspar.Compiler.Signature as S

type Ann a = S.Ann F.Data a
type Signature a = S.Signature F.Data a
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
The purpose of Feldspar is to implement high-performance software, especially in the domain of signal processing in embedded systems, see reference [@persson2014towards].

Feldspar comes with an optimizing compiler that translates Feldspar expressions into C99 code.[^FeldsparCompilerHackage]
When translating a function signature, the compiler uses a specific calling convention as detailed in chapter 1.4.2 in reference [@persson2014towards]:

- All functions return `void`{.c}
- Scalar values are passed by value
- Structured values (structs, arrays) are passed by reference
- Arrays are represented using a data structure `struct array`{.c}
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
``` {.ghci .c}
cgenProto $ lam $ \xs -> ptr "fft" $ fft xs
```
where `struct array`{.c} is a Feldspar specific data structure with metadata, such as the number of elements, and a pointer to the data area.

The Feldspar compiler uses its calling convention for a number of reasons, but the primary reasons are consistency and generality. The convention ensures that all arguments fit into a register, which helps avoid spilling arguments to the call stack. By passing arrays as references bundled with their length, the compiler can generate code that works with different array sizes and still preserve the same number of arguments.

A hard-wired set of mapping rules can be restrictive and introduce performance penalties. Code generated from Feldspar will be part of a larger system, and the calling convention is naturally dictated by the system rather than by the Feldspar compiler.



## Issues with Fixed Mappings

With a fixed signature mapping, it is easy to derive the target language type from the source language type. But the fixed mapping leaves little room to change the generated signature to fit into existing software. Instead, separate wrapper functions have to be written and maintained.

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
scProd as bs = F.desugar $ F.scalarProd (F.sugar as F.-:: F.tPull1 id)
                                        (F.sugar bs F.-:: F.tPull1 id)
```
The generated signature with the default mapping is:

``` {.ghci .c}
cgenProto $ lam $ \as -> lam $ \bs -> ptr "scProd" $ scProd as bs
```
By default, the Feldspar compiler automatically makes up names for the arguments.
Apart from the problem that Feldspar's `struct array`{.c} is an unconventional array representation, this code may also be considered too general: it has to cater for the fact that the arrays may have different lengths. Since it does not make sense to call `scProd` with arrays of different lengths, a more appropriate signature might be:

``` {.ghci .c}
cgenProto $ name "len" $ \len -> native len $ \as -> native len $ \bs -> ret "scProd" $ scProd as bs
```
Here, the arrays are passed as two pointers to the corresponding data buffers and a single length argument. This signature is more likely to occur in a practical system, and it has the advantage that the function does not have to decide what to do if the lengths are different. However, the system may expect a different order of the arguments, and might expect the result to be passed by value instead of by reference.

In addition to being able to customize the calling convention, we might also want to affect non-functional aspects of functions.
For example, we can name arguments for readability and debugging purposes.
This is helpful since Feldspar is an embedded language and that syntactic information is lost when the Haskell compiler reads the source file.

In future work we want to extend the annotations to include attributes to help the C compiler, including `restrict`{.c} and `volatile`{.c}.



## Contributions

To address the problems above, this paper presents two contributions:

- We define a simple EDSL to specify type conversions and annotations when exporting a Feldspar function to an external system (\cref{the-signature-language}).
- We give an implementation of the EDSL as a small wrapper around the existing Feldspar compiler (\cref{implementation}). The implementation relies on a simple interface to the underlying compiler.
- A generalized version of the implementation and the interface are provided as part of the `imperative-edsl` package.



# The Signature Language

Dissatisfied with hard-wired rules and global compiler options, we propose a small language as a more flexible way to drive the compiler.

The Signature language allows the programmer to express the mapping of individual arguments separately.
Specifically it allows the programmer to add annotations to every argument and control the data representation.
These annotations can be as simple as just giving a name to a parameter, using the `name` combinator.
Or, it can change the arity of the function by introducing new parameters, like the `native` and `exposeLength` combinators in \cref{combinators}.

Like the Feldspar language, the Signature language is a typed embedded domain specific language, embedded in Haskell.
The Signature language preserves the type safety of the Felspar language.

The Signature language interface is given in \cref{lst:signature-shallow}.
The combinators `lam` and `name` are used to bind (and possibly annotate) an argument, while `ret` and `ptr` are used to return the result of the function to be generated.

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
ex1 :: Signature ([Double] -> [Double] -> Double)
ex1 = lam $ \xs -> lam $ \ys -> ptr "scProd" (scProd xs ys)
```
which generates the following C signature when compiled
``` {.ghci .c}
cgenProto ex1
```

Using `name` instead of `lam`, we change the embedding to name the first argument
``` {.haskell}
ex2 :: Signature ([Double] -> [Double] -> Double)
ex2 = name "xs" $ \xs -> lam $ \ys -> ptr "scProd" (scProd xs ys)
```
resulting in
``` {.ghci .c}
cgenProto ex2
```

Finally, we change the function to return by value, by using `ret` instead of `ptr`
``` {.haskell}
ex3 :: Signature ([Double] -> [Double] -> Double)
ex3 = name "xs" $ \xs -> name "ys" $ \ys -> ret "scProd" (scProd xs ys)
```
which produces

``` {.ghci .c}
cgenProto ex3
```


The basic constructors in the language are useful for simple annotations on the arguments.
But it is also possible to create constructors that will change the arity or introduce interface code into the embedded function.
The interface code can bridge different representation formats.

Without the `Signature` language, we would have to write a C wrapper around the generated function.
A wrapper written in C is not polymorphic, but declared with concrete types, like `int` or `double`.
In contrast, the Feldspar functions are often polymorphic and the concrete types are decided at compile time.
A handwritten wrapper would have to change for different concrete types, and thus becomes a maintenance burden.
Also, the wrapper code is a separate function and can not be optimized together with the generated code. In contrast, the `Signature` language combinators are applied before optimization and code generation, and the wrapper code fuses with the function.

For example, consider the `scProd`{.haskell} function again.
In earlier versions it suffered from two problems.

1. The two arrays may have different lengths and the generated code has to defensively calculate the minimum length (see line 6 below).
2. The arrays are passed using a `struct array`{.c} pointer which results in extra dereferencing (the `at` macros in line 9 below).

``` {.ghci .c}
cgenDefinition $ lam $ \as -> lam $ \bs -> ptr "scProd" $ scProd as bs
```

To help alleviate these problems we can define smart constructors that modify the code before optimization.
Note that these smart constructors are extensions to the `Signature` language and can be expressed by the end user.

``` {.haskell .skip #combinators caption="Smart signature constructors"}
-- | Pass the argument as a native array of length @len@
native :: (Type a)
       => Data Length -> (Data [a] -> Signature b) -> Signature ([a] -> b)
native l f = Lam (Native l) $ \a -> f $ setLength l a

-- | Expose the length of an array
exposeLength :: (Type a)
             => (Data [a] -> Signature b) -> Signature (F.Length -> [a] -> b)
exposeLength f = name "len" $ \l -> native l f
```

The `native`{.haskell} function changes the array type to a native C array with length `l`.
By using the Feldspar `setLength`{.haskell} function, size information is added to the array arguments.
In \cref{implementation} we show how the `Native` constructor produces the interface code needed to translate between native and `struct array`{.c} formats.

The `exposeLength`{.haskell} function adds an extra length argument to the signature and passes this length to `native`. The effect is to break up a standard array argument into two arguments: a length and a native array.

With our new combinators, we can create a version of the `scProd` function that accepts native arrays of a fixed (runtime specified) length.

``` {.haskell}
scProdNative = name "len" $ \len ->
               native len $ \as  ->
               native len $ \bs  ->
               ret "scProd" $ scProd as bs
```
which compiles to:
``` {.ghci .c}
cgenDefinition scProdNative
```

Note how the Feldspar compiler now realizes that both vectors have the same length, and thus removes the defensive minimum length calculation.

The first two declarations in the generated code are for converting the native array in the interface to `struct array`{.c} which is what the body of the function expects. In the future, we plan to make it possible to use native arrays throughout the generated code, when stated so in the signature, but that requires a change to the Feldspar compiler and is out of scope for this paper.




# Implementation

The language is implemented as a combination of a shallow and a deep embedding.
The shallow embedding (\cref{lst:signature-shallow}), which is also the programmer interface, provides combinators to describe the mapping of a function.
The deep embedding (\cref{lst:signature-deep}) is interpreted by the compiler to apply the rules.

By using two separate embeddings it is possible to have a small set of constructs that the compiler has to deal with, while at the same time provide a rich set of combinators to the end user. For example, the `exposeLength` function could be implemented purely in terms of simpler constructs. This way of combining deep and shallow embeddings has been shown to be very powerful for implementing EDSLs [@svenningsson2013combining].

In this paper we show the implementation specialized to the Feldspar language.
A generalized version of the implementation is provided as part of the `imperative-edsl` library.

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

We can think of `Signature` as adding top-level lambda abstraction and result annotations to the existing expression language `Data`. The use of a host-language function in the `Lam` constructor is commonly known as *higher-order abstract syntax* (HOAS) [@pfenning1988higher]. HOAS allows us to construct signatures without the need to generate fresh variable names. As we will see in \cref{code-generation}, names are instead generated when we generate code from the signature.

## Code generation

`Signature` is defined as a wrapper type around the Feldspar expression type `Data`. In order to generate code for signatures, we first need to be able to generate code for `Data`. To this end, the Feldspar compiler provides the following interface:

``` {.haskell .skip}
varExp    :: Type a             => VarId -> Data a
compExp   :: (MonadC m)         => Data a -> m C.Exp
compTypeF :: (MonadC m, Type a) => proxy a -> m C.Type
```

The first function, `varExp`, is used to create a free variable in Feldspar. Naturally, this function is not exported to ordinary users. The function `compExp` is used to compile a Feldspar expression to a C expression `Exp`. Since compilation normally results in a number of C statements in addition to the expression, `compExp` returns in a monad `m` capable of collecting C statements that can later be pretty printed as C code. Finally, `compTypeF` is used to generate a C type from a type `a` constrained by Feldspar's `Type` class. The argument of type `proxy a` is just used to determine the type `a`.

``` {.haskell .skip #lst:translate-sig caption="Signature translation" style=floathere}
-- | Compile a @Signature@ to C code
translateFunction :: forall m a. (MonadC m) => Signature a -> m ()
translateFunction sig = go sig (return ())
  where
    go :: forall d. Signature d -> m () -> m ()
    go (Ret n a) prelude = do
      t <- compTypeF a
      inFunctionTy t n $ do
        prelude
        e <- compExp a
        addStm [cstm| return $e; |]
    go (Ptr n a) prelude = do
      t <- compTypeF a
      inFunction n $ do
        prelude
        e <- compExp a
        addParam [cparam| $ty:t *out |]
        addStm [cstm| *out = $e; |]
    go fun@(Lam Empty f) prelude = do
      t <- compTypeF (argProxy fun)
      v <- varExp <$> freshId
      C.Var n _ <- compExp v
      go (f v) $ prelude >> addParam [cparam| $ty:t $id:n |]
    go fun@(Lam n@(Native l) f) prelude = do
      t <- compTypeF (elemProxy n fun)
      i <- freshId
      let w = varExp i
      C.Var (C.Id m _) _ <- compExp w
      let n = m ++ "_buf"
      withAlias i ('&':m) $ go (f w) $ do
        prelude
        len <- compExp l
        addLocal [cdecl| struct array $id:m = { .buffer = $id:n
                                              , .length=$len
                                              , .elemSize=sizeof($ty:t)
                                              , .bytes=sizeof($ty:t)*$len
                                              }; |]
        addParam [cparam| $ty:t * $id:n |]
    go fun@(Lam (Named s) f) prelude = do
      t <- compTypeF (argProxy fun)
      i <- freshId
      withAlias i s $ go (f $ varExp i) $ prelude >> addParam [cparam| $ty:t $id:s |]

    argProxy :: Signature (b -> c) -> Proxy b
    argProxy _ = Proxy

    elemProxy :: Ann [b] -> Signature ([b] -> c) -> Proxy b
    elemProxy _ _ = Proxy
```


The code generator is defined in \cref{lst:translate-sig}. Before explaining how it works, we will explain the code generation technique used.

We use a C code generation monad for producing the C code. Operations of this monad are accessed via the `MonadC` type class. Among other things, it provides a method for generating fresh names, methods for adding statements to the generated code and for adding parameters to the currently generated function definition.

The concrete pieces of C code to be generated are written as actual C code using quasi-quoters\ [@mainland2007nice] for C code, provided by the package `language-c-quote`[^language-c-quote].

[^language-c-quote]: <http://hackage.haskell.org/package/language-c-quote>

For example, consider the following two lines from \cref{lst:translate-sig}:

``` {.haskell .skip firstnumber=17}
        addParam [cparam| $ty:t *out |]
        addStm [cstm| *out = $e; |]
```

The first line adds a parameter to the generated C function, and the second line adds a statement that assigns the result to the output pointer. The `[q| ... |]` syntax is for quasi-quotation, where `q` is the name of the quoter. The quoter parses the C code inside the brackets, and turns it into a representation of a piece of code that can be collected in the code generation monad.

Quasi-quoters also allow the splicing of Haskell values into the quoted code. In the above example, `$ty:t` splices in the Haskell value `t` as a C type, and `$e` splices in `e` as a C expression. For the code to type check, `t` must have the type `C.Type` and `e` must have the type `C.Exp`.

The signature is compiled by recursively traversing the `Lam` constructors and building up the argument list.
Finally, the `Ret` or `Ptr` case combines the arguments to produce the function signature.
The compilation of the function body is delegated to the Feldspar compiler (by calling `compExp`).

The `Lam (Native l)` case (lines 24--38 from \cref{lst:translate-sig}) is an example of how the `Signature` language can generate interface code.
``` {.haskell .skip firstnumber=24}
    go fun@(Lam n@(Native l) f) prelude = do
      t <- compTypeF (elemProxy n fun)
      i <- freshId
      let w = varExp i
      C.Var (C.Id m _) _ <- compExp w
      let n = m ++ "_buf"
      withAlias i ('&':m) $ go (f w) $ do
        prelude
        len <- compExp l
        addLocal [cdecl| struct array $id:m = { .buffer = $id:n
                                              , .length=$len
                                              , .elemSize=sizeof($ty:t)
                                              , .bytes=sizeof($ty:t)*$len
                                              }; |]
        addParam [cparam| $ty:t * $id:n |]
```
Apart from allocating a fresh parameter, it creates a local `struct array`{.c} object (lines 33--37) on the function stack and initializes it with the length `l` and the buffer parameter.
Then compilation continues with `f` applied to the address of the local `struct array`{.c} object.


# Related Work

MATLAB Coder [@matlab-coder][^MatlabCoder] is a tool that generates standalone C and C++ code from MATLAB code. One purpose of MATLAB Coder is to export MATLAB functions to an external system. Since MATLAB is dynamically typed, the same function can operate on values of different type. When generating C code, the user must specify a type for the function, and optionally sizes or size bounds for matrix arguments. This can be done on the command line using what can be seen as a restricted DSL.

However, judging from code examples provided by MathWorks, the signature mapping of MATLAB Coder appears to be rather restricted. For example, stack allocated matrices are passed as two arguments: a pointer to a data buffer and a length vector. If a static size is given for the matrix, the length vector goes away. But if a different argument order is needed, or if one wants to use the same length vector for two different matrices, this likely requires introducing a wrapper function with a different interface.

  <!-- See http://se.mathworks.com/help/fixedpoint/ug/c-code-interface-for-unbounded-arrays-and-structure-fields.html -->

[^MatlabCoder]: Matlab Coder <http://www.mathworks.com/products/matlab-coder>



# Discussion and Future Work

The `Signature` language enables us to customize the signature of compiled Feldspar functions.
It also allows generation of interface code fused with the original function.

Why is a new language needed?
Why not just add annotations to the `Lam`{.haskell} abstraction constructor in the Feldspar Core language?

The `Signature` language is a proper extension of the Feldspar Core language, which means it is optional and can co-exist with other extensions.
Since the `Signature` is built using a combination of deep and shallow embedding, the language is possible to extend by the end user.
Also, the `Signature` language can be seen as a replacement for the top-level lambda abstractions in the Feldspar expression.

In future work, we will generalize the `Signature` language to work with any expression language that supports the same interface (see \cref{code-generation}) as the Feldspar compiler.



# Acknowledgements {-}

This research is funded by the Swedish Foundation for Strategic
Research (which funds the Resource Aware Functional Programming (RAW
FP) Project) and the Swedish Research Council.

# References
