{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
   MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Pure.Capability.Aspect where

import Pure.Capability.Context
import Pure.Capability.Context.Trans (ContextT(..))
import Pure.State (PureM,MonadSRef(..),liftPure)

import Control.Monad.Reader as R hiding (lift)
import Control.Monad.IO.Class

import Data.Coerce
import Pure.Capability.FFunctor

newtype Aspect c r s a = Aspect 
  { runAspect :: ContextT c (ReaderT r (PureM s)) a 
  } deriving 
      (Functor,Applicative,Monad,MonadIO ,MonadReader r
      ,MonadSRef s
      ,MonadContext c
      )

class Rebase c m where
  rebase :: c m -> m (c IO)

prepare0 :: (Coercible b (Aspect c r s a)) => b -> Aspect c r s (IO a)
prepare0 x = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ liftPure s (runReaderT (runContextT (runAspect (coerce x)) c) r)

prepare1 :: (Coercible b (Aspect c r s a)) 
         => (g -> b) 
         -> Aspect c r s (g -> IO a)
prepare1 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 -> 
    liftPure s (runReaderT (runContextT (runAspect (coerce f _1)) c) r)

prepare2 :: (Coercible b (Aspect c r s a)) 
         => (g -> h -> b) 
         -> Aspect c r s (g -> h -> IO a)
prepare2 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 -> 
    liftPure s (runReaderT (runContextT (runAspect (coerce f _1 _2)) c) r)

prepare3 :: (Coercible b (Aspect c r s a)) 
         => (g -> h -> i -> b) 
         -> Aspect c r s (g -> h -> i -> IO a)
prepare3 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 -> 
    liftPure s (runReaderT (runContextT (runAspect (coerce f _1 _2 _3)) c) r)

prepare4 :: (Coercible b (Aspect c r s a)) 
         => (g -> h -> i -> j -> b) 
         -> Aspect c r s (g -> h -> i -> j -> IO a)
prepare4 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 -> 
    liftPure s (runReaderT (runContextT (runAspect (coerce f _1 _2 _3 _4)) c) r)

prepare5 :: (Coercible b (Aspect c r s a)) 
         => (g -> h -> i -> j -> k -> b) 
         -> Aspect c r s (g -> h -> i -> j -> k -> IO a)
prepare5 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 -> 
    liftPure s (runReaderT (runContextT (runAspect (coerce f _1 _2 _3 _4 _5)) c) r)

prepare6 :: (Coercible b (Aspect c r s a)) 
         => (g -> h -> i -> j -> k -> l -> b) 
         -> Aspect c r s (g -> h -> i -> j -> k -> l -> IO a)
prepare6 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 -> 
    liftPure s (runReaderT (runContextT (runAspect (coerce f _1 _2 _3 _4 _5 _6)) c) r)

prepare7 :: (Coercible b (Aspect c r s a)) 
         => (g -> h -> i -> j -> k -> l -> m -> b) 
         -> Aspect c r s (g -> h -> i -> j -> k -> l -> m -> IO a)
prepare7 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 _7 -> 
    liftPure s (runReaderT (runContextT (runAspect (coerce f _1 _2 _3 _4 _5 _6 _7)) c) r)

prepare8 :: (Coercible b (Aspect c r s a)) 
         => (g -> h -> i -> j -> k -> l -> m -> n -> b) 
         -> Aspect c r s (g -> h -> i -> j -> k -> l -> m -> n -> IO a)
prepare8 f = do
  r <- R.ask
  c <- ctx
  s <- sref
  pure $ \_1 _2 _3 _4 _5 _6 _7 _8 -> 
    liftPure s (runReaderT (runContextT (runAspect (coerce f _1 _2 _3 _4 _5 _6 _7 _8)) c) r)

