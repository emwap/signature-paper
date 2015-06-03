---
title: Programmable Signatures
author:
- __Anders Persson__
- Emil Axelsson
- Chalmers University of Technology
- Ericsson AB
date:
- TFP 2015
...

# Feldspar

<div style='float:left;width:45%;' class='centered'>
- Embedded Domain Specific Language

- High performance
- Portable
- Embedded
- Generates C99

</div>

<div style='float:right;width:55%;' class='centered'>

``` {.haskell}
makeCrcTable :: Bits a
             => Data a -> Pull DIM1 a
makeCrcTable poly = indexed1 256 $ \i ->
    forLoop 8 (i2n i .<<. (sz - 8)) step
  where
    sz       = bitSize poly
    step _ r =
      let r' = r .<<. 1
      in condition (tstBit r (sz-1))
                   (r' `xor` polynomial)
                   r'
```
</div>

# Calling Convention

```haskell
crc :: Data Word16 -> Data Word16 -> Data [Word8] -> Data Word16
```
is translated to

```c
void crc(uint16_t v0, uint16_t v1, struct array * v3, uint16_t * out)
```

# Calling Convention

- Variable names are generated
- All functions return `void`{.c}
- Results are returned through pointers
- Scalars are passed by value
- Structured data types are passed by reference

```c
void crc(uint16_t v0, uint16_t v1, struct array * v3, uint16_t * out)
```

# Calling Convention

- Fixed mapping
- One-size-fits-*almost*-everyone

# Motivation

- _Export_ Feldspar functions to a C99 environment
- Name arguments for readability and debugging
- Control data representation from a performance perspective

# Contributions

- A Signature translation EDSL
- A reusable implementation to generate C code

# Signature Language

```haskell
-- | Bind argument in Signature
lam :: (VarPred exp a)
    => (exp a -> Signature exp b) -> Signature exp (a -> b)

-- | Name argument in Signature
name :: (VarPred exp a)
     => String -> (exp a -> Signature exp b) -> Signature exp (a -> b)

-- | Return by value (ret) or reference (ptr)
ret,ptr :: (VarPred exp a)
        => String -> exp a -> Signature exp a

-- | Unpack array argument into length and data in Signature
exposeLength :: (Type a)
             => (Data [a] -> Signature Data b)
             -> Signature Data (F.Length -> [a] -> b)
```

# Example -- CRC

```haskell
crcSig :: Signature Data (Word16 -> Word16 -> [Word8] -> Word16)
crcSig = lam $ \p ->
         lam $ \i ->
         lam $ \v ->
         ptr "crc" $ crcNaive p i v
```

. . .

```c
void crc(uint16_t v0, uint16_t v1, struct array* v2, uint16_t* out);
```

# Example -- CRC

```haskell
namedSig :: Signature Data (Word16 -> Word16 -> [Word8] -> Word16)
namedSig = name "poly" $ \p ->
           name "init" $ \i ->
           name "arr"  $ \v ->
           ret  "crc"  $ crcNaive p i v
```

. . .

```c
uint16_t crc(uint16_t poly, uint16_t init, struct array* arr);
```

# Example -- CRC

```haskell
sig :: Signature Data (Word16 -> Word16 -> Length -> [Word8] -> Word16)
sig = name "poly"  $ \p ->
      name "init"  $ \i ->
      exposeLength $ \v ->
      ret  "crc"   $ crcNaive p i v
```

. . .

```c
uint16_t crc(uint16_t poly, uint16_t init, uint32_t len, uint8_t* v3_buf);
```

# Implementation

``` {.haskell}
-- | Constraint on the types of variables in a given expression language
type family VarPred (exp :: * -> *) :: * -> Constraint

-- | General interface for compiling expressions
class CompExp exp where
    -- | Variable expressions
    varExp  :: VarPred exp a => VarId -> exp a

    -- | Compilation of expressions
    compExp :: (MonadC m) => exp a -> m Exp

    -- | Extract expression type
    compType :: forall m a
             .  (MonadC m, VarPred exp a)
             => exp a -> m Type
```

From the package `imperative-edsl`

# Implementation

```haskell
data Ann exp a where
  Empty  :: Ann exp a
  Native :: VarPred exp a => Ann exp [a]
  Named  :: String -> Ann exp a

data Signature exp a where
  Ret    :: (VarPred exp a) => String -> exp a -> Signature exp a
  Ptr    :: (VarPred exp a) => String -> exp a -> Signature exp a
  Lam    :: (VarPred exp a)
         => Ann exp a -> (exp a -> Signature exp b) -> Signature exp (a -> b)
```

# Implementation

``` {.haskell}
translateFunction :: forall exp a.  (CompExp exp) => Signature exp a -> CodeGen ()
translateFunction = go [] where
    go :: forall d. [C.Param] -> Signature exp d -> CodeGen ()
    go as (Ret n a) = do
      t <- compType a
      inFunctionTy t n $ do
        addParams $ reverse as
        e <- compExp a
        addStm [cstm| return $e; |]
    go as (Ptr n a) = do
      t <- compType a
      inFunction n $ do
        addParams $ reverse as
        e <- compExp a
        addParam [cparam| $ty:t *out |]
        addStm [cstm| *out = $e; |]
-- other cases elided
```

# Conclusion

- A Signature translation language
- A reusable implementation based on `imperative-edsl`

</br>
</br>

### Future work

- Stacking of signature annotations

# Thank You

</br>
</br>

feldspar.github.io

<anders.persson@chalmers.se>

<emax@chalmers.se>
