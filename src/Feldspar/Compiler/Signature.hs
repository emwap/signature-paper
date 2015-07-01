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

compTypeF :: (MonadC m, Type a) => proxy a -> m C.Type
compTypeF = compTypePP (Proxy :: Proxy Data)

-- * Combinators

lam :: (Type a)
    => (Data a -> Signature b) -> Signature (a -> b)
lam f = Lam Empty $ \x -> f x

name :: (Type a)
     => String -> (Data a -> Signature b) -> Signature (a -> b)
name s f = Lam (Named s) $ \x -> f x

ret,ptr :: (Type a)
        => String -> Data a -> Signature a
ret = Ret
ptr = Ptr

arg :: (Type a)
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
cgenProto sig = ppr $ _prototypes $ snd $ runCGen (translateFunction sig) env
  where
    env = defaultCEnv Flags

cgenDefinition :: Signature a -> Doc
cgenDefinition sig = ppr $ _globals $ snd $ runCGen (translateFunction sig) env
  where
    env = defaultCEnv Flags

-- * Compilation

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
