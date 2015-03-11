---
documentclass: llncs
title: Programmable Signatures
author: Anders Persson
abstract: |
  Abstract goes here

biblio-files: paper.md
header-includes:
  \institute{Chalmers University of Technology \and Ericsson}
---


# Introduction

## Background

- Compiling from a typed embedded language
- Fixed mapping from host language type to target language type
- A fixed mapping is too restrictive

## Contributions

- An EDSL to specify the conversion of types when embedding a function in an EDSL
- An implementation of the EDSL as part of the Feldspar System Layer

``` {.haskell}
data Top a where
  Res :: Type b => Data b -> Top a
  Lam :: Type b => (Data b -> Top f) -> Top (a -> f)
```

``` {.haskell}
lam :: Type b => (Data b -> a) -> (a -> Top f) -> Top (a -> f)
lam s f = Lam $ f . s

res :: Type b => (a -> Data b) -> a -> Top a
res d = Res . d

arg :: (Syntax a, Type b) => (Data b -> Data (Internal a)) -> (a -> Top f) -> Top (a -> f)
arg f = lam (sugar . f)
```


# The concern of fixed mappings

Many compilers provide options to change the interpretation of program elements. However, these options take effect for the entire invocation of the compiler, meaning that it is not possible to apply options only to some elements.

- Compare with pragmas, such as `inline`, `UNPACK`, `restrict`, `volatile`, etc.

# Related Work

- How do they deal with this in Delite?
- pragmas?
- marks in MDD?
- REPA has a polymorphic parameter in the `Array` type
- Pull and Push arrays
- Matlab Coder has some example based thing to specialize the type
