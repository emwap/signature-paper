module Examples where

import qualified Prelude

import Feldspar hiding (name)
import Feldspar.Vector
import Feldspar.Algorithm.CRC
import Feldspar.Compiler.Signature

-- * Examples

expr1 :: Data [Index] -> Data Index
expr1 x = 1 + getLength x

expr3 :: Data Index -> Data Word8 -> Data Index
expr3 a b = a * i2n b

expr5 :: Data [Index] -> Data Index
expr5 v = let l = getLength v
           in forLoop l l $ \i s -> s + v ! i

sig6_1, sig6_2 :: Signature Data (Word16 -> Word16 -> [Word8] -> Word16)
sig6_1 = lam $ \p -> lam $ \i -> lam $ \v -> ptr "crc" $ crcNaive p i $ sugar v

sig6_2 = name "poly" $ \p ->
         name "ini"  $ \i ->
         name "arr"  $ \v ->
         ret  "crc"  $ crcNaive p i $ sugar v

sig6_3 :: Signature Data (Word16 -> Word16 -> Length -> [Word8] -> Word16)
sig6_3 = name "poly"   $ \p ->
         name "ini"    $ \i ->
         exposeLength  $ \v ->
         ret  "crc"    $ crcNaive p i $ sugar v

crc :: (Bits a, Integral a)
    => Data a -> Data a -> Data [Word8] -> Data a
-- crc p i v = share (makeCrcTable p)
--           $ \t -> crcNormal t i $ sugar v
crc p i v = crcNaive p i $ sugar v

sig7 :: Signature Data (Word16 -> [Word8] -> Word16)
sig7 = name "ini" $ \i ->
       name "vec" $ \v ->
       ret  "crc" $ crc 0x8005 i v

sig8_1 :: Signature Data ([Word8] -> [Word8] -> Word8)
sig8_1 = lam $ \as ->
         lam $ \bs ->
         ret "scalarProd" $ scalarProd (sugar as -:: tPull1 id)
                                       (sugar bs -:: tPull1 id)

sig8_2 :: Signature Data (Length -> [Word8] -> [Word8] -> Word8)
sig8_2 = name "len" $ \len ->
         native len $ \as ->
         native len $ \bs ->
         ret "scalarProd" $ scalarProd (sugar as -:: tPull1 id)
                                       (sugar bs -:: tPull1 id)
