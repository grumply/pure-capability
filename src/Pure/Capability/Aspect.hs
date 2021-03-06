{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
   MultiParamTypeClasses, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Pure.Capability.Aspect where

import Pure.Capability.Context
import Pure.Capability.Context.Trans (ContextT(..))
import Pure.Capability.FFunctor
import Pure.Data.View (View)
import Pure.State

import Control.Monad.Catch
import Control.Monad.Reader as R hiding (lift)
import Control.Monad.IO.Class

import Data.Coerce
import Data.Typeable

newtype Aspect c r s a = Aspect 
  { unAspect :: ContextT c (ReaderT r (PureM s)) a 
  } deriving 
      (Functor,Applicative,Monad,MonadIO,MonadReader r
      ,MonadFix
      ,MonadSRef s
      ,MonadContext c
      ,MonadCatch
      ,MonadThrow
      ,MonadMask
      )

evalAspect :: Aspect c r s a -> c -> r -> SRef s -> IO a
evalAspect f c r s = liftPure s (runReaderT (runContextT (unAspect f) c) r)

viewAspectWith :: Typeable s => Reactive -> Aspect c r s View -> c -> r -> s -> View
viewAspectWith dyn v c r s = runPureWith dyn s (runReaderT (runContextT (unAspect v) c) r)

viewAspect :: Typeable s => Aspect c r s View -> c -> r -> s -> View
viewAspect = viewAspectWith (Reactive False True)

viewAspectStatic :: Typeable s => Aspect c r s View -> c -> r -> s -> View
viewAspectStatic = viewAspectWith (Reactive False False)

class Rebase c m where
  rebase :: c m -> m (c IO)

prepare0 :: (Coercible f (Aspect c r s), MonadSRef s f, MonadContext c f, MonadReader r f) => f a -> f (IO a)
prepare0 x = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ evalAspect (coerce x) c r s 

prepare1 :: (Coercible f (Aspect c r s), MonadSRef s f, MonadContext c f, MonadReader r f) 
        => (g -> f a) 
        -> f (g -> IO a)
prepare1 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 -> evalAspect (coerce (f _1)) c r s 

prepare2 :: (Coercible f (Aspect c r s), MonadSRef s f, MonadContext c f, MonadReader r f) 
        => (g -> h -> f a) 
        -> f (g -> h -> IO a)
prepare2 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 -> evalAspect (coerce (f _1 _2)) c r s 

prepare3 :: (Coercible f (Aspect c r s), MonadSRef s f, MonadContext c f, MonadReader r f) 
        => (g -> h -> i -> f a) 
        -> f (g -> h -> i -> IO a)
prepare3 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 -> evalAspect (coerce (f _1 _2 _3)) c r s 

prepare4 :: (Coercible f (Aspect c r s), MonadSRef s f, MonadContext c f, MonadReader r f) 
        => (g -> h -> i -> j -> f a) 
        -> f (g -> h -> i -> j -> IO a)
prepare4 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 -> evalAspect (coerce (f _1 _2 _3 _4)) c r s 

prepare5 :: (Coercible f (Aspect c r s), MonadSRef s f, MonadContext c f, MonadReader r f) 
        => (g -> h -> i -> j -> k -> f a) 
        -> f (g -> h -> i -> j -> k -> IO a)
prepare5 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 -> evalAspect (coerce (f _1 _2 _3 _4 _5)) c r s 

prepare6 :: (Coercible f (Aspect c r s), MonadSRef s f, MonadContext c f, MonadReader r f) 
        => (g -> h -> i -> j -> k -> l -> f a) 
        -> f (g -> h -> i -> j -> k -> l -> IO a)
prepare6 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 -> evalAspect (coerce (f _1 _2 _3 _4 _5 _6)) c r s 

prepare7 :: (Coercible f (Aspect c r s), MonadSRef s f, MonadContext c f, MonadReader r f) 
        => (g -> h -> i -> j -> k -> l -> m -> f a) 
        -> f (g -> h -> i -> j -> k -> l -> m -> IO a)
prepare7 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 _7 -> evalAspect (coerce (f _1 _2 _3 _4 _5 _6 _7)) c r s 

prepare8 :: (Coercible f (Aspect c r s), MonadSRef s f, MonadContext c f, MonadReader r f) 
        => (g -> h -> i -> j -> k -> l -> m -> n -> f a) 
        -> f (g -> h -> i -> j -> k -> l -> m -> n -> IO a)
prepare8 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 _7 _8 -> evalAspect (coerce (f _1 _2 _3 _4 _5 _6 _7 _8)) c r s 

