{-# LANGUAGE RankNTypes, KindSignatures, MultiParamTypeClasses,
   FunctionalDependencies, CPP, FlexibleInstances, UndecidableInstances #-}
module Pure.Capability.Trans (
    -- * The Context monad
    Context,
    context,
    runContext,
    mapContext,
    withContext,
    -- * The ContextT monad transformer
    ContextT(..),
    mapContextT,
    withContextT,
    -- * Context operations
    ctx,
    contextual,
    ctxs
    ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class

#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant
#endif

import Data.Functor.Identity

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Functor

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

type Context c a = ContextT c Identity a

context :: (Monad m) => (c m -> a) -> ContextT c m a
context f = ContextT (return . f)
{-# INLINE context #-}

runContext :: Context c a -> c Identity -> a
runContext m = runIdentity . runContextT m
{-# INLINE runContext #-}

mapContext :: (a -> b) -> Context c a -> Context c b
mapContext f = mapContextT id (Identity . f . runIdentity)
{-# INLINE mapContext #-}

withContext :: (c' Identity -> c Identity) -> Context c a -> Context c' a
withContext = withContextT
{-# INLINE withContext #-}

newtype ContextT c m a = ContextT { runContextT :: c m -> m a }

mapContextT :: (c n -> c m) -> (m a -> n b) -> ContextT c m a -> ContextT c n b
mapContextT g f m = ContextT (f . runContextT m . g)
{-# INLINE mapContextT #-}

withContextT :: (c' m -> c m) -> ContextT c m a -> ContextT c' m a
withContextT f m = ContextT $ runContextT m . f
{-# INLINE withContextT #-}

instance Functor m => Functor (ContextT c m) where
  fmap f = mapContextT id (fmap f)
  {-# INLINE fmap #-}
  x <$ v = mapContextT id (x <$) v
  {-# INLINE (<$) #-}

instance Applicative m => Applicative (ContextT c m) where
  pure = liftContextT . pure
  {-# INLINE pure #-}
  f <*> v = ContextT $ \c -> runContextT f c <*> runContextT v c
  {-# INLINE (<*>) #-}
  u  *> v = ContextT $ \c -> runContextT u c  *> runContextT v c
  {-# INLINE (*>) #-}
  u <*  v = ContextT $ \c -> runContextT u c <*  runContextT v c
  {-# INLINE (<*) #-}
  liftA2 f x y = ContextT $ \c -> liftA2 f (runContextT x c) (runContextT y c)
  {-# INLINE liftA2 #-}

instance Alternative m => Alternative (ContextT c m) where
  empty = liftContextT empty
  {-# INLINE empty #-}
  m <|> n = ContextT $ \c -> runContextT m c <|> runContextT n c
  {-# INLINE (<|>) #-}

instance Monad m => Monad (ContextT c m) where
  m >>= k = ContextT $ \c -> do
    a <- runContextT m c
    runContextT (k a) c
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
#if !(MIN_VERSION_base(4,13,0))
  fail msg = lift (fail msg)
  {-# INLINE fail #-}
#endif

instance Fail.MonadFail m => Fail.MonadFail (ContextT c m) where
  fail msg = lift (Fail.fail msg)
  {-# INLINE fail #-}

instance MonadPlus m => MonadPlus (ContextT c m) where
  mzero = lift mzero
  {-# INLINE mzero #-}
  m `mplus` n = ContextT $ \c -> runContextT m c `mplus` runContextT n c
  {-# INLINE mplus #-}

instance MonadFix m => MonadFix (ContextT c m) where
  mfix f = ContextT $ \c -> mfix $ \a -> runContextT (f a) c
  {-# INLINE mfix #-}

instance MonadTrans (ContextT c) where
  lift = liftContextT
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (ContextT c m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadZip m => MonadZip (ContextT c m) where
  mzipWith f (ContextT m) (ContextT n) = ContextT $ \c -> mzipWith f (m c) (n c)

#if MIN_VERSION_base(4,12,0)
instance Contravariant m => Contravariant (ContextT c m) where
  contramap f = ContextT . fmap (contramap f) . runContextT
#endif

liftContextT :: m a -> ContextT c m a
liftContextT m = ContextT (const m)
{-# INLINE liftContextT #-}

ctx :: Monad m => ContextT c m (c m)
ctx = ContextT return
{-# INLINE ctx #-}

contextual :: (c m -> c m) -> ContextT c m a -> ContextT c m a
contextual = withContextT
{-# INLINE contextual #-}

ctxs :: Monad m => (c m -> a) -> ContextT c m a
ctxs f = ContextT (return . f)
{-# INLINE ctxs #-}

instance MonadReader r m => MonadReader r (ContextT c m) where
  ask = lift ask
  local = mapContextT id . local

instance (Monoid w, MonadWriter w m) => MonadWriter w (ContextT c m) where
  writer = lift . writer
  tell = lift . tell
  listen = mapContextT id listen
  pass = mapContextT id pass

instance MonadState s m => MonadState s (ContextT c m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadCont m => MonadCont (ContextT c m) where
  callCC = liftCallCC callCC

instance MonadError e m => MonadError e (ContextT c m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

liftCallCC :: (((a -> m b) -> m a) -> m a) 
           -> ((a -> ContextT c m b) -> ContextT c m a) 
           -> ContextT c m a
liftCallCC callCC f = 
  ContextT $ \c ->
    callCC $ \cc ->
      runContextT (f (\a -> ContextT $ \_ -> cc a)) c

liftCatch :: (m a -> (e -> m a) -> m a) 
          -> ContextT c m a 
          -> (e -> ContextT c m a) 
          -> ContextT c m a
liftCatch f m h = ContextT $ \c -> f (runContextT m c) (\e -> runContextT (h e) c)
