{-# LANGUAGE RankNTypes, KindSignatures, MultiParamTypeClasses,
   FunctionalDependencies, CPP #-}
module Pure.Capability.FFunctor where

-- | FFunctor is a specialization of the concept of `Functor` applied to types 
-- of kind `(* -> *) -> *`
class FFunctor (w :: (* -> *) -> *) where
  ffmap :: (forall a. m a -> n a) -> w m -> w n

lift0 = ($)
lift1 = (.)
lift2 = lift1.(.)
lift3 = lift2.(.)
lift4 = lift3.(.)
lift5 = lift4.(.)
lift6 = lift5.(.)
lift7 = lift6.(.)
lift8 = lift7.(.)

