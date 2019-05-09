{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses,
   UndecidableInstances #-}
module Pure.Capability.Context (
    MonadContext(..),
    ctxs,
    ) where

import Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Error
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as ReaderT (ask, local, reader)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, ask, local, reader)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, ask, local, reader)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

import Control.Monad.Trans.Class (lift)
import Control.Monad
import Data.Monoid

import Pure.Capability.Context.Trans (ContextT)
import qualified Pure.Capability.Context.Trans as ContextT

class Monad m => MonadContext c m | m -> c where
  ctx :: m c
  ctx = context id

  contextual :: (c -> c) -> m a -> m a

  context :: (c -> a) -> m a
  context f = do
    c <- ctx
    return (f c)

  {-# MINIMAL (ctx | context), contextual #-}

ctxs :: MonadContext c m => (c -> a) -> m a
ctxs = context
{-# INLINE ctxs #-}

instance MonadContext c ((->) c) where
  ctx            = id
  contextual f m = m . f
  context        = id

instance Monad m => MonadContext c (ContextT c m) where
  ctx        = ContextT.ctx
  contextual = ContextT.contextual
  context    = ContextT.context

instance MonadContext c' m => MonadContext c' (ContT r m) where
  ctx        = lift ctx
  contextual = Cont.liftLocal ctx contextual
  context    = lift . context

instance (Error e, MonadContext c m) => MonadContext c (ErrorT e m) where
  ctx        = lift ctx
  contextual = mapErrorT . contextual
  context    = lift . context

instance MonadContext c m => MonadContext c (ExceptT e m) where
  ctx        = lift ctx
  contextual = mapExceptT . contextual
  context    = lift . context

instance MonadContext c m => MonadContext c (IdentityT m) where
  ctx        = lift ctx
  contextual = mapIdentityT . contextual
  context    = lift . context

instance MonadContext c m => MonadContext c (ListT m) where
  ctx        = lift ctx
  contextual = mapListT . contextual
  context    = lift . context

instance MonadContext c m => MonadContext c (MaybeT m) where
  ctx        = lift ctx
  contextual = mapMaybeT . contextual
  context    = lift . context

instance MonadContext c m => MonadContext c (Lazy.StateT s m) where
  ctx        = lift ctx
  contextual = Lazy.mapStateT . contextual
  context    = lift . context

instance MonadContext c m => MonadContext c (Strict.StateT s m) where
  ctx        = lift ctx
  contextual = Strict.mapStateT . contextual
  context    = lift . context

instance (Monoid w, MonadContext c m) => MonadContext c (Lazy.WriterT w m) where
  ctx        = lift ctx
  contextual = Lazy.mapWriterT . contextual
  context    = lift . context

instance (Monoid w, MonadContext c m) => MonadContext c (Strict.WriterT w m) where
  ctx        = lift ctx
  contextual = Strict.mapWriterT . contextual
  context    = lift . context

