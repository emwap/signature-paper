module Examples where

import Feldspar (Data)
import qualified Feldspar as F
import qualified Feldspar.Vector as F
import qualified Feldspar.Algorithm.CRC as F
import Feldspar.Compiler.Signature

-- * Examples

expr1 :: Data [F.Index] -> Data F.Index
expr1 x = 1 + F.getLength x

expr3 :: Data F.Index -> Data F.Word8 -> Data F.Index
expr3 a b = a * F.i2n b

expr5 :: Data [F.Index] -> Data F.Index
expr5 v = let l = F.getLength v
           in F.forLoop l l $ \i s -> s + v F.! i

sig6_1, sig6_2 :: Signature (F.Word16 -> F.Word16 -> [F.Word8] -> F.Word16)
sig6_1 = lam $ \p -> lam $ \i -> lam $ \v -> ptr "crc" $ F.crcNaive p i $ F.sugar v

sig6_2 = name "poly" $ \p ->
         name "ini"  $ \i ->
         name "arr"  $ \v ->
         ret  "crc"  $ F.crcNaive p i $ F.sugar v

sig6_3 :: Signature (F.Word16 -> F.Word16 -> F.Length -> [F.Word8] -> F.Word16)
sig6_3 = name "poly"   $ \p ->
         name "ini"    $ \i ->
         exposeLength  $ \v ->
         ret  "crc"    $ F.crcNaive p i $ F.sugar v

crc :: F.Bits a
    => Data a -> Data a -> Data [F.Word8] -> Data a
-- crc p i v = F.share (F.makeCrcTable p)
--           $ \t -> F.crcNormal t i $ F.sugar v
crc p i v = F.crcNaive p i $ F.sugar v

sig7 :: Signature (F.Word16 -> [F.Word8] -> F.Word16)
sig7 = name "ini" $ \i ->
       name "vec" $ \v ->
       ret  "crc" $ crc 0x8005 i v

sig8_1 :: Signature ([F.Word8] -> [F.Word8] -> F.Word8)
sig8_1 = lam $ \as ->
         lam $ \bs ->
         ret "scalarProd" $ F.scalarProd (F.thawPull1 as) (F.thawPull1 bs)

sig8_2 :: Signature (F.Length -> [F.Word8] -> [F.Word8] -> F.Word8)
sig8_2 = name "len" $ \len ->
         native len $ \as ->
         native len $ \bs ->
         ret "scalarProd" $ F.scalarProd (F.thawPull1 as) (F.thawPull1 bs)
