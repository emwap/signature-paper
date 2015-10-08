{-# LANGUAGE FlexibleContexts #-}

module Feldspar.Compiler.Signature
  ( module Language.Embedded.Signature
  , native
  , exposeLength
  , capped
  , cgenSig
  , cgenProto
  , cgenDefinition
  )
  where

import Feldspar hiding (name)

-- import Control.Monad.Operational.Compositional
import Language.C.Monad
import Language.Embedded.Signature
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Imperative.Frontend
import Feldspar.Compiler.FromImperative () -- For @VarPred Data a@ instances

-- For examples
import Text.PrettyPrint.Mainland (ppr,pretty,Doc)

-- * Feldspar Combinators

native :: (Type a)
       => Data Length -> (Data [a] -> Signature Data b) -> Signature Data ([a] -> b)
native l f = Lam (Native l) $ \a -> f $ setLength l a

exposeLength :: (Type a)
             => (Data [a] -> Signature Data b) -> Signature Data (Length -> [a] -> b)
exposeLength f = name "len" $ \l -> native l f

capped :: (Type a)
       => Size a -> (Data a -> Signature Data b) -> Signature Data (a -> b)
capped sz = arg Empty (cap sz)


instance Show Doc where
  show = pretty 78

cgenSig :: (CompExp exp) => Signature exp a -> Doc
cgenSig = prettyCGen . translateFunction

cgenProto :: (CompExp exp) => Signature exp a -> Doc
cgenProto sig = ppr $ _prototypes $ snd $ runCGen (translateFunction sig) env
  where
    env = defaultCEnv Flags

cgenDefinition :: (CompExp exp) => Signature exp a -> Doc
cgenDefinition sig = ppr $ _globals $ snd $ runCGen (translateFunction sig) env
  where
    env = defaultCEnv Flags

