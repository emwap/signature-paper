---
title: Programmable Signatures
author:
- __Anders Persson__ <anders.persson@chalmers.se>
- Emil Axelsson <emax@chalmers.se>
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

```haskell
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

- Export Feldspar functions to a C99 environment
- Name arguments for readability
- Control data representation from a performance perspective

# Contributions

- A Signature translation EDSL
- An implementation for the Feldspar System Layer

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
void crc(uint16_t v0, uint16_t v1, struct array* v2, uint16_t* out)
{
    uint16_t e8;
    uint32_t len9;
    uint16_t e10;
    uint16_t v7;

    len9 = getLength(v2);
    e8 = v1;
    for (uint32_t v3 = 0; v3 < len9; v3 += 1) {
        e10 = (uint16_t) (uint32_t) ((uint8_t) (e8 >> 8) ^ at(uint8_t, v2,
                                                              v3)) << 8;
        for (uint32_t v5 = 0; v5 < 8; v5 += 1) {
            v7 = e10 << 1;
            if ((e10 & 32768) != 0) {
                e10 = v7 ^ v0;
            } else {
                e10 = v7;
            }
        }
        e8 = e10 ^ e8 << 8;
    }
    *out = e8;
}
```

# Example -- CRC

```haskell
namedSig :: Signature Data (Word16 -> Word16 -> [Word8] -> Word16)
namedSig = name "poly" $ \p ->
           name "init" $ \i ->
           name "arr"  $ \v ->
           ret "crc" $ crcNaive p i v
```

. . .

```c
uint16_t crc(uint16_t poly, uint16_t init, struct array* arr);
uint16_t crc(uint16_t poly, uint16_t init, struct array* arr)
{
    uint16_t e8;
    uint32_t len9;
    uint16_t e10;
    uint16_t v7;

    len9 = getLength(arr);
    e8 = init;
    for (uint32_t v3 = 0; v3 < len9; v3 += 1) {
        e10 = (uint16_t) (uint32_t) ((uint8_t) (e8 >> 8) ^ at(uint8_t, arr,
                                                              v3)) << 8;
        for (uint32_t v5 = 0; v5 < 8; v5 += 1) {
            v7 = e10 << 1;
            if ((e10 & 32768) != 0) {
                e10 = v7 ^ poly;
            } else {
                e10 = v7;
            }
        }
        e8 = e10 ^ e8 << 8;
    }
    return e8;
}
```

# Example -- CRC

```haskell
sig :: Signature Data (Word16 -> Word16 -> Length -> [Word8] -> Word16)
sig = name "poly"  $ \p ->
      name "init"  $ \i ->
      exposeLength $ \v ->
      ret "crc" $ crcNaive p i v
```

. . .

```c
uint16_t crc(uint16_t poly, uint16_t ini, uint32_t len, uint8_t* v3_buf);
uint16_t crc(uint16_t poly, uint16_t ini, uint32_t len, uint8_t* v3_buf)
{
    uint16_t e9;
    uint16_t e10;
    uint16_t v8;

    e9 = ini;
    for (uint32_t v4 = 0; v4 < len; v4 += 1) {
        e10 = (uint16_t) (uint32_t) ((uint8_t) (e9 >> 8) ^ at(uint8_t, v3,
                                                              v4)) << 8;
        for (uint32_t v6 = 0; v6 < 8; v6 += 1) {
            v8 = e10 << 1;
            if ((e10 & 32768) != 0) {
                e10 = v8 ^ poly;
            } else {
                e10 = v8;
            }
        }
        e9 = e10 ^ e9 << 8;
    }
    return e9;
}
```

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

```haskell
-- | Constraint on the types of variables in a given expression language
type family VarPred (exp :: * -> *) :: * -> Constraint

-- from the imperative-edsl package
```
# Thank You

</br>
</br>

feldspar.github.io

