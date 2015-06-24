{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Feldspar.Compiler.Signature where

import Data.Proxy
import Control.Applicative

import Feldspar (Data,Type,FeldOpts(..),defaultFeldOpts)

import qualified Language.C.Syntax as C

-- import Control.Monad.Operational.Compositional
import Language.C.Monad
import Language.C.Quote.C
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Imperative.Frontend

-- For examples
import qualified Feldspar as F
import qualified Feldspar.Vector as F
import qualified Feldspar.Algorithm.CRC as F
import Feldspar.Compiler.FromImperative ()
import Text.PrettyPrint.Mainland

-- TODO move Signature and translateFunction to feldspar-compiler-shim

data Ann exp a where
  Empty  :: Ann exp a
  Native :: VarPred exp a => Data F.Length -> Ann exp [a]
  Named  :: String -> Ann exp a

data Signature exp a where
  Ret    :: (VarPred exp a) => String -> exp a -> Signature exp a
  Ptr    :: (VarPred exp a) => String -> exp a -> Signature exp a
  Lam    :: (VarPred exp a)
         => Ann exp a -> (exp a -> Signature exp b) -> Signature exp (a -> b)

cgenSig :: (CompExp exp)
        => Signature exp a -> Doc
cgenSig = prettyCGen . translateFunction

translateFunction :: forall exp m a
                  .  (CompExp exp, MonadC m)
                  => Signature exp a -> m ()
translateFunction sig = go sig (return ())
  where
    go :: forall d. Signature exp d -> m () -> m ()
    go (Ret n a) body = do
      t <- compType a
      inFunctionTy t n $ do
        body
        e <- compExp a
        addStm [cstm| return $e; |]
    go (Ptr n a) body = do
      t <- compType a
      inFunction n $ do
        body
        e <- compExp a
        addParam [cparam| $ty:t *out |]
        addStm [cstm| *out = $e; |]
    go fun@(Lam Empty f) body = do
      t <- compTypePP (Proxy :: Proxy exp) (argProxy fun)
      v <- varExp <$> freshId
      C.Var n _ <- compExp v
      go (f v) $ body >> addParam [cparam| $ty:t $id:n |]
    go fun@(Lam n@(Native l) f) body = do
      t <- compTypePP (Proxy :: Proxy exp) (elemProxy n fun)
      w <- varExp <$> freshId
      C.Var m _ <- compExp w
      let n = appendId m "_buf"
      go (f w) $ do
        body
        len <- compExp l
        addLocal [cdecl| struct array $id:m = { .length = $len, .buffer = $id:n }; |]
        addParam [cparam| $ty:t * $id:n |]
    go fun@(Lam (Named s) f) body = do
      t <- compTypePP (Proxy :: Proxy exp) (argProxy fun)
      i <- freshId
      withAlias i s $ go (f $ varExp i) $ body >> addParam [cparam| $ty:t $id:s |]

    argProxy :: Signature exp (b -> c) -> Proxy b
    argProxy _ = Proxy

    elemProxy :: Ann exp [b] -> Signature exp ([b] -> c) -> Proxy b
    elemProxy _ _ = Proxy

    appendId :: C.Id -> String -> C.Id
    appendId (C.Id s loc) suf = C.Id (s++suf) loc


-- * Combinators

lam :: (VarPred Data a)
    => (Data a -> Signature Data b) -> Signature Data (a -> b)
lam f = Lam Empty $ \x -> f x

name :: (VarPred Data a)
     => String -> (Data a -> Signature Data b) -> Signature Data (a -> b)
name s f = Lam (Named s) $ \x -> f x

ret,ptr :: (VarPred Data a)
        => String -> Data a -> Signature Data a
ret = Ret
ptr = Ptr

arg :: (VarPred Data a)
    => Ann Data a -> (Data a -> Data b) -> (Data b -> Signature Data c) -> Signature Data (a -> c)
arg s g f = Lam s $ \x -> f (g x)

-- * Feldspar Combinators

native :: (Type a)
       => Data F.Length -> (Data [a] -> Signature Data b) -> Signature Data ([a] -> b)
native l f = Lam (Native l) $ \a -> f $ F.setLength l a

exposeLength :: (Type a)
             => (Data [a] -> Signature Data b) -> Signature Data (F.Length -> [a] -> b)
exposeLength f = name "len" $ \l -> native l f

capped :: (Type a) => F.Size a -> (Data a -> Signature Data b) -> Signature Data (a -> b)
capped sz = arg Empty (F.cap sz)


