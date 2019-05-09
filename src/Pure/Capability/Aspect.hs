{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
   MultiParamTypeClasses, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Pure.Capability.Aspect where

import Pure.Capability.Context
import Pure.Capability.Context.Trans (ContextT(..))
import Pure.State (PureM,MonadSRef(..),liftPure,SRef)

import Control.Monad.Reader as R hiding (lift)
import Control.Monad.IO.Class

import Data.Coerce
import Pure.Capability.FFunctor

newtype Aspect c r s a = Aspect 
  { unAspect :: ContextT c (ReaderT r (PureM s)) a 
  } deriving 
      (Functor,Applicative,Monad,MonadIO ,MonadReader r
      ,MonadSRef s
      ,MonadContext c
      )

runAspect :: Aspect c r s a -> c -> r -> SRef s -> IO a
runAspect f c r s = liftPure s (runReaderT (runContextT (unAspect f) c) r)

class Rebase c m where
  rebase :: c m -> m (c IO)

prepare0 x = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ runAspect c r s (coerce x)

prepare1 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 -> runAspect c r s (coerce f _1)

prepare2 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 -> runAspect c r s (coerce f _1 _2)

prepare3 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 -> runAspect c r s (coerce f _1 _2 _3)

prepare4 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 -> runAspect c r s (coerce f _1 _2 _3 _4)

prepare5 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 -> runAspect c r s (coerce f _1 _2 _3 _4 _5)

prepare6 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 -> runAspect c r s (coerce f _1 _2 _3 _4 _5 _6)

prepare7 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 _7 -> runAspect c r s (coerce f _1 _2 _3 _4 _5 _6 _7)

prepare8 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 _7 _8 -> runAspect c r s (coerce f _1 _2 _3 _4 _5 _6 _7 _8)

