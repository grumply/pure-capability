{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Pure.Capability.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Given a single-constructor non-GADT parametrically polymorphic record type
-- of fields with parameterized return types, produce a `Monad<Capability>`
-- class to simplify use of that type. This is a sort of inversion/reification
-- of the MTL approach of specifying capabilities implicitly w.r.t. the
-- evaluation context. Instead, we supply the capabilities explicitly, allowing
-- for modification/extension a la reification/reflection.
--
-- Example:
--
-- > data SomeCapability m = SomeCapability { _methodA :: Int -> m Bool }
-- > mkCapability ''SomeCapability
-- 
-- Produces:
--
-- > class MonadSomeCapability m where
-- >   withSomeCapability :: (SomeCapability m -> m a) -> m a
-- >   
-- >   methodA :: Int -> m Bool
-- >   methodA i = withSomeCapability $ \sc -> _methodA sc i
--
-- Where you would only be required to supply an instance of
-- `withSomeCapability` to use `MonadSomeCapability` in your evaluation 
-- context.
--
-- See `Pure.Capability` for an idea of how to group capabilities into a context
-- a la MonadReader.
--
-- With this approach, given nested contexts, you can lower some capability to a
-- shared base context before passing it to a nested evaluation context. For 
-- instance, you could add an effect, say logging, to the methods of a 
-- capability from the supplier of the capability. If the capability is shared
-- cascade-style from the root context through the nested evaluation contexts,
-- you could add communicative effects that notify/communicate with parent 
-- contexts on each or specific method calls.
--
-- This concept of nested evaluation contexts is significant for the style of 
-- evaluation seen in pure applications, where nesting of contexts is
-- especially pervasive.
--
-- This is not the most expressive solution to this problem of supplied and 
-- shared evaluation contexts, but it strikes a nice balance between simplicity
-- and power.
--
-- NOTE: This approach enforces the constraint `Monad m =>` for the capabilities
-- parameter even though that constraint is not technically required. This
-- simplifies instantiation of the class in the common case, but might not be 
-- desired. If you find a case in which this is not desired, let me know and we
-- can rework this approach with some parameterization.
mkCapability :: Name -> Q [Dec]
mkCapability record = do
  info <- reify record
  case info of
    TyConI (DataD _ _ [] _ _ _) ->
      error "error in mkCapability: capabilities must have at least one type variable with a last type variable of kind `* -> *`"
    TyConI (DataD _ (Name (OccName capability) _) tyvs _ [RecC _ methods] _) ->
      case last tyvs of
        KindedTV tyNm (AppT (AppT ArrowT StarT) StarT) -> do
          ctx <- newName "m"
          let 
            -- Context doesn't technically need to be a monad, but I don't 
            -- see any usage being non-monadic, so we call it
            -- `Monad<Capability>`
            className = mkName ("Monad" ++ capability) 
            ltyvs = fmap lowerTVB (init tyvs)
          result <- newName "a"
          mthds <- makeMethods ltyvs capability ctx result methods
          pure $ 
            [ ClassD 

              -- doesn't technically need this, but it simplifies types of 
              -- implementations of capabilities and I don't envisage any 
              -- capabilities being non-monadic
              [ AppT (ConT ''Monad) (VarT ctx) ] 

              className 
              ( ltyvs ++ [ PlainTV ctx ] )
              [] 
              mthds
            ]
        _ -> error "error in mkCapability: the last type variable of a capability must be of kind `* -> *`"
    _ ->
      error "error in mkCapability: capabilities must be parametrically polymorphic single-constructor non-GADT record data type with a last type variable of kind `* -> *`"
  where
    makeMethods tyvs capability ctx result [] =
      let 
        capTy = buildCapabilityTy (ConT (mkName capability)) tyvs ctx
        withName = mkName ("with" ++ capability)
        resultType = AppT (VarT ctx) (VarT result)
        useType = AppT (AppT ArrowT capTy) resultType
       in 
        pure [ SigD withName ( AppT (AppT ArrowT useType) resultType ) ]
    makeMethods tyvs capability ctx result ( (mthdNm@(Name (OccName mthd) _),_,ty) : rest ) = 
      case mthd of
        ('_':mthd') -> do
          vars  <- mkFunVars ty
          rest' <- makeMethods tyvs capability ctx result rest
          let from = extractContext ty
          pure $
            [ SigD (mkName mthd') (rewriteContext from ctx ty)
            , FunD (mkName mthd') 
              [ Clause vars 
                (NormalB (VarE (mkName ("with" ++ capability)) `AppE` 
                            LamE [ VarP (mkName "c") ] 
                              (saturate (VarE mthdNm `AppE` VarE (mkName "c")) vars)
                         ) 
                )
                []
              ]
            ] ++ rest'
        _ -> makeMethods tyvs capability ctx result rest

    buildCapabilityTy f [] ctx = f `AppT` (VarT ctx)
    buildCapabilityTy f (PlainTV nm : rest) ctx  = buildCapabilityTy (f `AppT` (VarT nm)) rest ctx

    mkFunVars (ForallT _ _ (AppT (AppT ArrowT l) r)) = do
      x <- newName "x"
      xs <- mkFunVars r
      pure (VarP x : xs)
    mkFunVars (ForallT _ _ (AppT l r)) = pure []
    mkFunVars (AppT (AppT ArrowT l) r) = do
      x <- newName "x"
      xs <- mkFunVars r
      pure (VarP x : xs)
    mkFunVars (AppT l r) = pure []

    extractContext (ForallT _ _ ty) = extractContext ty
    extractContext (AppT (AppT ArrowT _) r) = extractContext r
    extractContext (AppT (VarT l) _) = l

    rewriteContext from to (VarT x) 
      | x == from = VarT to
      | otherwise = VarT x
    rewriteContext from to (ForallT tvbs ctx' ty) = 
      let tvbs' = fmap lowerTVB tvbs
       in ForallT tvbs' ctx' (rewriteContext from to ty)
    rewriteContext from to (AppT l r) = AppT (rewriteContext from to l) (rewriteContext from to r)
    rewriteContext from to x = x

    lowerTVB (KindedTV nm _) = PlainTV nm
    lowerTVB x = x

    saturate f [] = f
    saturate f [ VarP x ] = f `AppE` (VarE x)
    saturate f (VarP x : vars) = saturate (f `AppE` (VarE x)) vars

