{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, RankNTypes, NoMonomorphismRestriction #-}
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
  -- * The Aspect type
  Aspect(..),
  evalAspect,
  viewAspectWith,
  viewAspectStatic,
  viewAspect,
  -- * The Rebase class
  Rebase(..),
  -- * The prepare methods
  prepare0,
  prepare1,
  prepare2,
  prepare3,
  prepare4,
  prepare5,
  prepare6,
  prepare7,
  prepare8,
  -- * Re-exported modules
  module Control.Monad,
  module Control.Monad.Fix,
  module Control.Monad.Reader,
  module Control.Monad.Trans,
  module Control.Monad.Catch,
  module Pure.State
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

import Control.Monad.Catch

import Control.Monad.Reader

import Pure.State

{-
import Pure.Capability.TH
import Control.Monad.IO.Class
import Control.Monad.Reader as R
import Pure.State (MonadSRef(..),liftPure)

data X m = X 
  { _method1 :: forall n. Num n => n -> m ()
  }
mkCapability ''X

data Ctx m = Ctx
  { x :: X m
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

