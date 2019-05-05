{-# LANGUAGE RankNTypes, KindSignatures, MultiParamTypeClasses,
   FunctionalDependencies, CPP, FlexibleInstances #-}
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

import Pure.Capability.Class
import Pure.Capability.FFunctor
import Pure.Capability.Trans (
  Context, runContext, mapContext, withContext,
  ContextT(ContextT), runContextT, mapContextT, withContextT)

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans

