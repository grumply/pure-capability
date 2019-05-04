{-# LANGUAGE RankNTypes, KindSignatures, MultiParamTypeClasses,
   FunctionalDependencies #-}
module Pure.Capability where

{-| The classes in this module are a convenience for use and manipulation of 
    records of capabilities parameterized on their field's evaulation contexts. 

    See Pure.Capability.TH for a method of deriving `Monad<Capability>` classes;
    the other half of this architectural approach.
-}

-- | FFunctor is a specialization of the concept of `Functor` applied to types 
-- of kind `(* -> *) -> *`
class FFunctor (w :: (* -> *) -> *) where
  ffmap :: (forall a. m a -> n a) -> w m -> w n

-- | MonadContext is a specialization of MonadReader for contextualized
-- computation with a parametrically polymorphic environment specialized 
-- to the evaluation context:
--
-- > instance MonadReader (env m) m where
--
-- This is separated from MonadReader for moral differentiation.
class Monad m => MonadContext (c :: (* -> *) -> *) m | m -> c where
  -- | Retrieves the monad context.
  ctx :: m (c m)
  ctx = context id

  -- | Executes a computation in a modified context.
  contextual :: (c m -> c m) -- ^ The function to modify the context.
             -> m a          -- ^ Computation to run in the modified context.
             -> m a

  -- | Retrieves a function of the current context.
  context :: (c m -> a) 
          -> m a
  context f = do
    c <- ctx
    return (f c)

  {-# MINIMAL (ctx | context), contextual #-}

-- instance MonadReader (r m) m => MonadContext r m where
--   ctx = ask
--   contextual = local

-- | Retrieves a function of the current context.
ctxs :: MonadContext c m 
     => (c m -> a) -- ^ The selector function to apply to the context.
     -> m a
ctxs = context



