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
---


\newcommand{\todo}[1]{\marginpar{\scriptsize\textcolor{red}{TODO~{#1}}}}

\todo{Indent code listings}

# Introduction

  <!--
- Compiling from a typed embedded language
- Fixed mapping from host language type to target language type
- A fixed mapping is too restrictive
  -->

The Feldspar language [@axelsson2010feldspar; @axelsson2010design] is an embedded domain specific language written in Haskell.
The purpose of Feldspar is to implement high-performance software, especially in the domain of signal processing in embedded systems [@persson2014towards].

Feldspar comes with an optimizing compiler that translates Feldspar expressions into C99 code.
When translating a function signature, the compiler uses a specific calling convention as detailed in [@persson2014towards, ch. 1.4.2]:

- Scalar values are passed by value
- Structured values (structs, arrays) are passed by reference
- Arrays are represented using a data structure `struct array`{.C}
- All functions return `void`{.C}
- Return values are passed through caller provided pointers

For example, a Feldspar function with type

``` {.haskell}
fft :: Data [Double] -> Data [Double]
```
is translated into the C99 signature

``` {.C}
void fft(struct array *, struct array * *);
```
where `struct array`{.C} is a Feldspar specific data structure with metadata and a pointer to the data area.

The Feldspar compiler uses this convention for a number of reasons, but the primary reason is consistency.

By passing arrays as references bundled with their length, the Feldspar Compiler can generate code that works with different array sizes and still preserve the same number of arguments.


However, a hard-wired set of mapping rules can be restrictive and introduce performance penalties.
For example, to access the data buffer in a `struct array *`{.C} an extra pointer dereference is required as compared to a normal array access.

## Issues with Fixed Mappings

With a fixed signature mapping it is easy to derive the target language type from the source language type. But the fixed mapping leaves little room to change the generated signature to fit into existing software. Instead, separate wrappers have to be written and maintained.

Many compilers provide options to change the interpretation of program elements.
However, these options take effect for the entire invocation of the compiler, meaning that it is not possible to apply options only to some elements.

\todo{Describe the problem better}

- Compare with pragmas, such as `inline`, `UNPACK`, `restrict`, `volatile`, etc.

## Contributions

This paper presents two contributions.

- An EDSL to specify the conversion of types when embedding a function in host EDSL.

- An implementation of the EDSL as part of the Feldspar System Layer

# The Signature Language

Dissatisfied with hard-wired rules and global compiler options, we propose a small language as a more flexible way to drive the compiler.
The signature language allows the programmer to express the mapping of individual arguments separately.

The basic combinators `arg` and `res`, are used for argument positions and the result respectively.

``` {.haskell #lst:sig-lang caption="Signature language"}
arg :: (VarPred exp a)
    => Maybe String -> (exp a -> Signature exp b) -> Signature exp (a -> b)
res :: (VarPred exp a)
    => Bool -> String -> exp a -> Signature exp a
```


As a running example, we use the following Feldspar function which takes an array of words into a single word.

``` {.haskell}
fun :: Data [Word32] -> Data Word32
```

We can mimic the standard rules of the Feldspar compiler by wrapping the function in our combinators.

``` {.haskell}
lam = arg Nothing     -- | Capture an argument without naming
ptr = res False       -- | Return by reference

ex1 = embed $ lam $ \x -> ptr "fun" (fun x)
```
which generates the following C signature when compiled
``` {.C|
void fun(struct array * v0, uint32_t * out);
```

We change the embedding to name the first argument
``` {.haskell}
ex2 = embed $ arg (Just "vec") $ \x -> ptr "fun" (fun x)
```
resulting in
``` {.C}
void fun(struct array * vec, uint32_t * out);
```

Finally, we change the function to return by value
``` {.haskell}
ret = res True        -- | Return by value

ex3 = embed $ arg (Just "vec") $ \x -> ret "fun" (fun x)
```
which produces

``` {.C}
uint32_t fun(struct array * vec);
```

# Implementation

The language is implemented using the technique of combining deep and shallow embeddings [@svenningsson2013combining].

The shallow embedding, which is also the programmer interface, provides combinators to describe the mapping of a function.
The deep embedding is interpreted by the compiler to apply the rules.

By using two separate embeddings it is possible to have a small set of constructs that the compiler has deal with, while at the same time provide a rich set of combinators to the end user.

``` {.haskell}
-- | Annotations to place on arguments or result
data Ann = AsValue Bool
         | Name String

-- | Annotation carrying signature description
data Signature exp a where
  Res :: (VarPred exp a)
      => [Ann] -> String -> exp a -> Signature exp a
  Lam :: (VarPred exp a)
      => [Ann] -> (exp a -> Signature exp b)
      -> Signature exp (a -> b)

arg Nothing     = Lam []
arg (Just name) = Lam [Name name]

res asVal = Res [AsValue asVal]
```

The signature is compiled by recursively traversing the `Lam` constructors and building up the argument list.
Finally, the `Res` node is compiled and combined with the arguments to produce the function signature.
The compilation of the function body is delegated to the Feldspar compiler.

The final paper will show in more detail how the signature is compiled into C code.

# Evaluation

# Related Work

The final paper will consider techniques in other languages and systems, e.g. Scala Delite, the use of marks in model driven design and Matlab Coder.

# References
