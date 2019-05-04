{-# LANGUAGE RankNTypes, KindSignatures, MultiParamTypeClasses,
   FunctionalDependencies, CPP #-}
module Pure.Capability.FFunctor (FFunctor(..)) where

-- | FFunctor is a specialization of the concept of `Functor` applied to types 
-- of kind `(* -> *) -> *`
class FFunctor (w :: (* -> *) -> *) where
  ffmap :: (forall a. m a -> n a) -> w m -> w n

