{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
   MultiParamTypeClasses, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Pure.Capability.Aspect where

import Pure.Capability.Context
import Pure.Capability.Context.Trans (ContextT(..))
import Pure.Capability.FFunctor
import Pure.Data.View (View)
import Pure.State

import Control.Monad.Reader as R hiding (lift)
import Control.Monad.IO.Class

import Data.Coerce
import Data.Typeable

newtype Aspect c r s a = Aspect 
  { unAspect :: ContextT c (ReaderT r (PureM s)) a 
  } deriving 
      (Functor,Applicative,Monad,MonadIO ,MonadReader r
      ,MonadSRef s
      ,MonadContext c
      )

evalAspect :: Aspect c r s a -> c -> r -> SRef s -> IO a
evalAspect f c r s = liftPure s (runReaderT (runContextT (unAspect f) c) r)

viewAspectWith :: Typeable s => Reactive -> Aspect c r s View -> c -> r -> s -> View
viewAspectWith dyn v c r s = runPureWith dyn s (runReaderT (runContextT (unAspect v) c) r)

viewAspect :: Typeable s => Aspect c r s View -> c -> r -> s -> View
viewAspect = viewAspectWith (Reactive False False)

viewAspectDyn :: Typeable s => Aspect c r s View -> c -> r -> s -> View
viewAspectDyn = viewAspectWith (Reactive True True)

class Rebase c m where
  rebase :: c m -> m (c IO)

prepare0 x = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ evalAspect (coerce x) c r s 

prepare1 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 -> evalAspect (coerce (f _1)) c r s 

prepare2 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 -> evalAspect (coerce (f _1 _2)) c r s 

prepare3 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 -> evalAspect (coerce (f _1 _2 _3)) c r s 

prepare4 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 -> evalAspect (coerce (f _1 _2 _3 _4)) c r s 

prepare5 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 -> evalAspect (coerce (f _1 _2 _3 _4 _5)) c r s 

prepare6 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 -> evalAspect (coerce (f _1 _2 _3 _4 _5 _6)) c r s 

prepare7 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 _7 -> evalAspect (coerce (f _1 _2 _3 _4 _5 _6 _7)) c r s 

prepare8 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 _7 _8 -> evalAspect (coerce (f _1 _2 _3 _4 _5 _6 _7 _8)) c r s 

