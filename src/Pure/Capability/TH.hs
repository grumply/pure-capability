{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Pure.Capability.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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
      error "error in mkCapability: capabilities must be single-constructor non-GADT record types with at least one type variable with a last type variable of kind `* -> *`"
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

