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

-- * Combinators

lam :: (VarPred Data a)
    => (Data a -> Signature b) -> Signature (a -> b)
lam f = Lam Empty $ \x -> f x

name :: (VarPred Data a)
     => String -> (Data a -> Signature b) -> Signature (a -> b)
name s f = Lam (Named s) $ \x -> f x

ret,ptr :: (VarPred Data a)
        => String -> Data a -> Signature a
ret = Ret
ptr = Ptr

arg :: (VarPred Data a)
    => Ann a -> (Data a -> Data b) -> (Data b -> Signature c) -> Signature (a -> c)
arg s g f = Lam s $ \x -> f (g x)

-- * Feldspar Combinators

native :: (Type a)
       => Data F.Length -> (Data [a] -> Signature b) -> Signature ([a] -> b)
native l f = Lam (Native l) $ \a -> f $ F.setLength l a

exposeLength :: (Type a)
             => (Data [a] -> Signature b) -> Signature (F.Length -> [a] -> b)
exposeLength f = name "len" $ \l -> native l f

capped :: (Type a) => F.Size a -> (Data a -> Signature b) -> Signature (a -> b)
capped sz = arg Empty (F.cap sz)


-- * Language
data Ann a where
  Empty  :: Ann a
  Native :: Type a => Data F.Length -> Ann [a]
  Named  :: String -> Ann a

data Signature a where
  Ret    :: (Type a) => String -> Data a -> Signature a
  Ptr    :: (Type a) => String -> Data a -> Signature a
  Lam    :: (Type a)
         => Ann a -> (Data a -> Signature b) -> Signature (a -> b)

cgenSig :: Signature a -> Doc
cgenSig = prettyCGen . translateFunction

cgenProto :: Signature a -> Doc
cgenProto sig = ppr $ cenvToCUnit cenv
  where
    env = defaultCEnv Flags
    cenv = onlyProto $ snd $ runCGen (translateFunction sig) env
    onlyProto e = env {_prototypes = _prototypes e}

-- * Compilation

-- | Compile a @Signature@ to C code
translateFunction :: forall m a
                  .  (MonadC m)
                  => Signature a -> m ()
translateFunction sig = go sig (return ())
  where
    go :: forall d. Signature d -> m () -> m ()
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
      t <- compTypePP (Proxy :: Proxy Data) (argProxy fun)
      v <- varExp <$> freshId
      C.Var n _ <- compExp v
      go (f v) $ body >> addParam [cparam| $ty:t $id:n |]
    go fun@(Lam n@(Native l) f) body = do
      t <- compTypePP (Proxy :: Proxy Data) (elemProxy n fun)
      w <- varExp <$> freshId
      C.Var m _ <- compExp w
      let n = appendId m "_buf"
      go (f w) $ do
        body
        len <- compExp l
        addLocal [cdecl| struct array $id:m = { .length = $len, .buffer = $id:n }; |]
        addParam [cparam| $ty:t * $id:n |]
    go fun@(Lam (Named s) f) body = do
      t <- compTypePP (Proxy :: Proxy Data) (argProxy fun)
      i <- freshId
      withAlias i s $ go (f $ varExp i) $ body >> addParam [cparam| $ty:t $id:s |]

    argProxy :: Signature (b -> c) -> Proxy b
    argProxy _ = Proxy

    elemProxy :: Ann [b] -> Signature ([b] -> c) -> Proxy b
    elemProxy _ _ = Proxy

    appendId :: C.Id -> String -> C.Id
    appendId (C.Id s loc) suf = C.Id (s++suf) loc

