{-# LANGUAGE RankNTypes, KindSignatures, MultiParamTypeClasses,
   FunctionalDependencies, CPP #-}
module Pure.Capability (
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
  module Control.Monad.Fix
  ) where

import Pure.Capability.FFunctor
import Pure.Capability.Trans (
  Context, runContext, mapContext, withContext,
  ContextT(ContextT), runContextT, mapContextT, withContextT)

import Control.Monad
import Control.Monad.Fix


