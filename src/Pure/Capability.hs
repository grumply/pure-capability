module Pure.Capability (
  -- * The Context Monad
  MonadContext(..),
  ctxs,
  -- * The FFunctor class
  FFunctor(..),
  -- * The Context monad
  Context,
  runContext,
  mapContext,
  withContext,
  -- * The ContextT monad transformer
  ContextT(ContextT),
  runContextT,
  mapContextT,
  withContextT,
  module Control.Monad,
  module Control.Monad.Fix,
  module Control.Monad.Trans
  ) where

import Pure.Capability.Aspect
import Pure.Capability.Context
import Pure.Capability.Context.Trans (
  Context, runContext, mapContext, withContext,
  ContextT(ContextT), runContextT, mapContextT, withContextT)
import Pure.Capability.FFunctor

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans

{-
data X a m = X 
  { _method1 :: a -> m () 
  , _method2 :: a -> Int -> m ()
  , _method3 :: a -> Int -> Int -> m ()
  , _method4 :: a -> Int -> Int -> Int -> m ()
  , _method5 :: a -> Int -> Int -> Int -> Int -> m ()
  , _method6 :: a -> Int -> Int -> Int -> Int -> Int -> m ()
  , _method7 :: a -> Int -> Int -> Int -> Int -> Int -> Int -> m () 
  , _method8 :: a -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> m () 
  }
mkCapability ''X

data Ctx m = Ctx
  { x :: X Int m
  }
mkContext ''Ctx

newtype SomeM a = SomeM 
  { runSomeM :: Aspect (Ctx SomeM) () () a 
  } deriving
    (Functor,Applicative,Monad,MonadIO
    ,MonadReader ()
    ,MonadSRef ()
    ,MonadContext (Ctx SomeM)
    )
mkAspect ''SomeM
-}
