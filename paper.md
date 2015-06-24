---
documentclass: llncs
title: Programmable Signatures
author:
  - Anders Persson
  - Emil Axelsson
abstract: |
  When compiling EDSLs into other languages, types in the source language must be translated into corresponding types in the target language.
  The translation is often driven by a small set of rules that map a single type in the source language into a single type in the target language.
  However, this simple approach is limiting when there are multiple possible mappings, and it may lead to poor interoperability and performance in the generated code.

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



# Introduction

  <!--
- Compiling from a typed embedded language
- Fixed mapping from host language type to target language type
- A fixed mapping is too restrictive
  -->

Feldspar is an embedded domain specific language written in Haskell [@axelsson2010feldspar; @axelsson2010design].
The purpose of Feldspar is to implement high-performance software, especially in the domain of signal processing in embedded systems [@persson2014towards].

Feldspar comes with an optimizing compiler that translates Feldspar expressions into C99 code.
When translating a function signature, the compiler uses a specific calling convention as detailed in [@persson2014towards, ch. 1.4.2]:

- Scalar values are passed by value
- Structured values (structs, arrays) are passed by reference
- Arrays are represented using a data structure `struct array`{.C}
- All functions return `void`{.C}
- Return values are passed through caller provided pointers

For example, the following is the type of an FFT function in Feldspar:

``` {.haskell}
fft :: Data [Double] -> Data [Double]
```
The type constructor `Data` denotes a Feldspar expression, and its parameter denotes the type of value computed by that expression. For historical reasons, Feldspar uses `[]` to denote immutable arrays.

When compiled, the `fft` is translated into the C99 signature

``` {.C}
void fft(struct array *, struct array * *);
```
where `struct array`{.C} is a Feldspar specific data structure with metadata, such as the number of elements, and a pointer to the data area.

The Feldspar compiler uses its calling convention for a number of reasons, but the primary reasons are consistency and generality. By passing arrays as references bundled with their length, the compiler can generate code that works with different array sizes and still preserve the same number of arguments.

However, a hard-wired set of mapping rules can be restrictive and introduce performance penalties. Code generated from Feldspar will usually be part of a larger system, and the calling convention is naturally dictated by the system rather than by the Feldspar compiler.



## Issues with Fixed Mappings

With a fixed signature mapping it is easy to derive the target language type from the source language type. But the fixed mapping leaves little room to change the generated signature to fit into existing software. Instead, separate wrapper functions have to be written and maintained.

In a typical embedded system, arrays are passed as two arguments: a pointer to the data buffer and an integer that gives the number of elements of the array. However, there are many variations on this theme. Should the length come before or after the buffer? Can the length argument be used for several arrays if they always have the same length? And so on.

Even if we allow flags to customize the compiler, it is clear that a fixed set of mapping rules will never be able to cover all possible situations. Instead, we would like to put the exported signature in the hands of the programmer.

  <!--
Many compilers provide options to change the interpretation of program elements.
However, these options take effect for the entire invocation of the compiler, meaning that it is not possible to apply options only to some elements.
  -->

As a concrete example, take the following function for computing the scalar product of two vectors:

``` {.haskell}
scProd :: Data [Double] -> Data [Double] -> Data Double
```
The generated signature with the default mapping is:

``` {.C}
void scProd(struct array * v0, struct array * v1, double * out);
```
(By default, the Feldspar compiler automatically makes up names for the arguments.)
Apart from the problem that Feldspar's `struct array` is an unconventional array representation, this code may also be considered too general: it has to cater for the fact that the arrays may have different lengths. Since it does not make sense to call `scProd` with arrays of different lengths, a more appropriate signature might be:

``` {.C}
void scProd(double * v0, double * v1, int length, double * out);
```
Here, the arrays are passed as two pointers to the corresponding data buffers and a single length argument. This signature is more likely to occur in a practical system, and it has the advantage that the function does not have to decide what to do if the lengths are different. However, the system may very well expect a different order of the arguments, and might expect the result to be passed by value instead of by reference.

In addition to being able to customize the calling convention, we might also want to affect non-functional aspects of functions.

\todo{Variable names, annotations: `inline`, `UNPACK`, `restrict`, `volatile`, etc.}



## Contributions

To address the problems above, this paper presents two contributions:

- We define a simple EDSL to specify type conversions and annotations when exporting a Feldspar function to an external system (section\ \ref{the-signature-language}).
- We give an implementation of the EDSL as a small wrapper around the existing Feldspar compiler (section\ \ref{implementation}). The implementation relies on a simple interface to the underlying compiler, and the technique can easily be ported to other EDSLs for which the compiler implements the same interface.



# The Signature Language

Dissatisfied with hard-wired rules and global compiler options, we propose a small language as a more flexible way to drive the compiler.
The signature language allows the programmer to express the mapping of individual arguments separately.

The basic combinators `arg` and `res`, are used for argument positions and the result respectively.

``` {.haskell .skip #lst:signature-shallow style=float caption="Signature language (shallow embedding)"}
-- | Capture an argument
lam :: (VarPred Data a)
    => (Data a -> Signature Data b) -> Signature Data (a -> b)

-- | Capture and name an argument
name :: (VarPred Data a)
     => String -> (Data a -> Signature Data b) -> Signature Data (a -> b)

-- | Create a named function return either by value or reference
ret,ptr :: (VarPred Data a)
        => String -> Data a -> Signature Data a
```


As a running example, we use the following Feldspar function which takes an array of words into a single word.

``` {.haskell}
fun :: Data [Word32] -> Data Word32
```

We can mimic the standard rules of the Feldspar compiler by wrapping the function in our combinators.

``` {.haskell}
lam = arg Nothing     -- | Capture an argument without naming
ptr = res False       -- | Return by reference

ex1 = lam $ \x -> ptr "fun" (fun x)
```
which generates the following C signature when compiled
``` {.C|
void fun(struct array * v0, uint32_t * out);
```

We change the embedding to name the first argument
``` {.haskell}
ex2 = named "vec" $ \x -> ptr "fun" (fun x)
```
resulting in
``` {.C}
void fun(struct array * vec, uint32_t * out);
```

Finally, we change the function to return by value
``` {.haskell}
ret = res True        -- | Return by value

ex3 = named "vec" $ \x -> ret "fun" (fun x)
```
which produces

``` {.C}
uint32_t fun(struct array * vec);
```


# Implementation

\todo{Describe the C-monad and CompExp}

The language is implemented using the technique of combining deep and shallow embeddings [@svenningsson2013combining].

The shallow embedding (\cref{lst:signature-shallow}), which is also the programmer interface, provides combinators to describe the mapping of a function.
The deep embedding (\cref{lst:signature-deep}) is interpreted by the compiler to apply the rules.

By using two separate embeddings it is possible to have a small set of constructs that the compiler has to deal with, while at the same time provide a rich set of combinators to the end user.

``` {.haskell .skip #lst:signature-deep style=float caption="Signature Language (deep embedding)"}
-- | Annotations to place on arguments or result
data Ann exp a where
  Empty  :: Ann exp a
  Native :: VarPred exp a => Data F.Length -> Ann exp [a]
  Named  :: String -> Ann exp a

-- | Annotation carrying signature description
data Signature exp a where
  Ret    :: (VarPred exp a) => String -> exp a -> Signature exp a
  Ptr    :: (VarPred exp a) => String -> exp a -> Signature exp a
  Lam    :: (VarPred exp a)
         => Ann exp a -> (exp a -> Signature exp b) -> Signature exp (a -> b)
```

The signature is compiled by recursively traversing the `Lam` constructors and building up the argument list.
Finally, the `Res` node is compiled and combined with the arguments to produce the function signature.
The compilation of the function body is delegated to the Feldspar compiler.

The final paper will show in more detail how the signature is compiled into C code.



# Discussion and Future Work

- Generialization of the Signature language is future work



# Related Work

The final paper will consider techniques in other languages and systems, e.g. Scala Delite, the use of marks in model driven design and Matlab Coder.

MATLAB Coder is a tool that generates standalone C and C++ code from MATLAB code. One purpose of MATLAB Coder is to export MATLAB functions to an external system. Since MATLAB is dynamically typed, the same function can operate on values of different type. When generating C code, the user must specify a type for the function, and optionally sizes or size bounds for matrix arguments. This can be done on the command line using what can be seen as a restricted DSL.

However, judging from the officially available examples, the signature mapping of MATLAB Coder appears to be rather restricted. For example, stack allocated matrices are passed as two arguments: a pointer to a data buffer and a length vector. If a static size is given for the matrix, the length vector goes away. But if a different argument order is needed, or if one wants to use the same length vector for two different matrices, this likely requires introducing a wrapper function with a different interface.

  <!-- See http://se.mathworks.com/help/fixedpoint/ug/c-code-interface-for-unbounded-arrays-and-structure-fields.html -->



# References
