{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}
module Pure.Capability.TH (mkCapability, mkContext, mkAspect, module Export) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Pure.Capability.Aspect as Export
import Pure.Capability.FFunctor as Export

import Data.Foldable
import Data.Traversable

import Debug.Trace
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
-- > instance FFunctor SomeCapability where
-- >   ffmap f c = c { _methodA = (lift1 f) (_methodA c) }
-- >
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
-- NOTE: This approach enforces the constraint `Monad m =>` for the parameter 
-- even though that constraint is not technically required. This simplifies 
-- instantiation of the class in the common case, but might not be desired. 
-- If you find a case in which this is not desired, let me know and we can 
-- rework this approach with some parameterization.
mkCapability :: Name -> Q [Dec]
mkCapability record = do
  info <- reify record
  case info of
    TyConI (DataD _ _ [] _ _ _) ->
      error "error in mkCapability: capabilities must have at least one type variable with a last type variable of kind `* -> *`"
    TyConI (DataD _ c@(Name (OccName capability) _) tyvs _ [RecC _ methods] _) ->
      case last tyvs of
        KindedTV tyNm (AppT (AppT ArrowT StarT) StarT) -> do
          monadCap <- makeMonadCapability capability tyvs methods
          ffunc <- ffunctor c (apply (ConT c) (init tyvs)) methods
          pure [ffunc,monadCap]
        _ -> error "error in mkCapability: the last type variable of a capability must be of kind `* -> *`"
    _ ->
      error "error in mkCapability: capabilities must be parametrically polymorphic single-constructor non-GADT record data type with a last type variable of kind `* -> *`"
  where
    apply f [] = f
    apply f ( PlainTV tyv : tyvs ) = apply (AppT f (VarT tyv)) tyvs
    apply f ( KindedTV tyv _ : tyvs ) = apply (AppT f (VarT tyv)) tyvs

    makeMonadCapability cap tyvs methods = do
      ctx <- newName "m"
      let 
        -- Context doesn't technically need to be a monad, but I don't 
        -- see any usage being non-monadic, so we call it
        -- `Monad<Capability>`
        className = mkName ("Monad" ++ cap) 
        ltyvs = fmap lowerTVB (init tyvs)

      result <- newName "a"
      mthds <- makeMethods ltyvs cap ctx result methods
      pure $ ClassD 

        -- doesn't technically need this, but it simplifies types of 
        -- implementations of capabilities and I don't envisage any 
        -- capabilities being non-monadic
        [ AppT (ConT ''Monad) (VarT ctx) ] 

        className 
        ( ltyvs ++ [ PlainTV ctx ] )
        [] 
        mthds
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

    ffunctor cap cty mthds = do
      f <- newName "f" 
      c <- newName "c"
      pure $ InstanceD Nothing [] (AppT (ConT (mkName "FFunctor")) cty) 
        [ FunD (mkName "ffmap") 
            [ Clause [ VarP f, VarP c ] (NormalB (RecUpdE (VarE c) (upds f c mthds))) [] ]
        ]
      where
        upds f c [] = []
        upds f c ((nm,_,mthd) : mthds) = 
          let 
            args = show (argCount 0 mthd)
            exp = VarE (mkName ("lift" ++ args)) `AppE` VarE f `AppE` (ParensE (VarE nm `AppE` VarE c))
          in 
            (nm,exp) : upds f c mthds
          where
            argCount n (AppT (AppT ArrowT _) r) = argCount (n + 1) r
            argCount n (ForallT _ _ r) = argCount n r
            argCount n _ = n

-- | Produces an FFunctor instance for a context record. 
--
-- > data X m = X { _methodA :: Int -> m () }
-- > data Ctx m = Ctx { x :: X m }
-- > mkContext ''Ctx
--
-- Produces:
--
-- > instance FFunctor Ctx where
-- >   ffmap f ctx = ctx { x = ffmap f (x ctx) }
--
-- The context type must not have any free variables other than the evaluation context
-- (`m` above).
--
mkContext :: Name -> Q [Dec]
mkContext record = do
  info <- reify record
  case info of
    TyConI (DataD _ _ [] _ _ _) ->
      error "error in capabilities: must have at least one type variable with a last type variable of kind `* -> *`"
    TyConI (DataD _ c@(Name (OccName capability) _) tyvs _ [RecC _ methods] _) ->
      case last tyvs of
        KindedTV tyNm (AppT (AppT ArrowT StarT) StarT) -> do
          f <- ffunctor c (apply (ConT c) (init tyvs)) methods
          pure [f]
        _ -> error "error in capabilities: last type variable must be of kind `* -> *`"
    _ ->
      error "error in capabilities: must be parametrically polymorphic single-constructor non-GADT record data type with a last type variable of kind `* -> *`"
  where
    apply f [] = f
    apply f ( PlainTV tyv : tyvs ) = apply (AppT f (VarT tyv)) tyvs
    apply f ( KindedTV tyv _ : tyvs ) = apply (AppT f (VarT tyv)) tyvs

    ffunctor cap cty mthds = do
      f <- newName "f" 
      c <- newName "c"
      pure $ InstanceD Nothing [] (AppT (ConT (mkName "FFunctor")) cty) 
        [ FunD (mkName "ffmap") 
            [ Clause [ VarP f, VarP c ] (NormalB (RecUpdE (VarE c) (upds f c mthds))) [] ]
        ]
      where
        upds f c [] = []
        upds f c ((nm,_,mthd) : mthds) = 
          let 
            exp = VarE (mkName "ffmap") `AppE` VarE f `AppE` (ParensE (VarE nm `AppE` VarE c))
          in 
            (nm,exp) : upds f c mthds


-- To pass one or more capabilities to a child view, the capabilities must often
-- be re-based for evaluation within the new context.  To accomplish this, the
-- methods of the capability must be evaluated down to IO and then lifted back
-- into the new context. For an individual method of a single argument, this 
-- would be:
--
-- > fmap liftIO . prepareN
--
-- But since a capability is generally a record of methods, each method has to
-- be rebased and the record reconstructed with the new fields.
-- 
-- Since this is tedious and rote, we rely on a th-derived instance of 
-- MonadRebase that is created alongside an instantiation of `mkCapability`.


-- | Produces:
--   * an instance of `Monad<capability>` for each capability in the Aspect's
--     context.
--   * an instance of `Rebase <capability> <aspect>` for each capability in the
--     Aspect's context.
--   * an instance of `Rebase <context> <aspect>` for the context of the aspect.
--
-- > data X a m = X { _methodA :: a -> m () }
-- > data Ctx m = Ctx { x :: X Int m }
-- > newtype SomeM a = SomeM { unSomeM :: Aspect Ctx () () a }
-- > mkAspect ''SomeM
--
-- Produces:
--
-- > instance MonadX Int SomeM where withX = (>>=) (ctxs x)
-- > instance Rebase X SomeM where 
-- >   rebase x = do 
-- >     c <- ctx
-- >     r <- ask
-- >     s <- sref
-- >     pure (X (evalAspect (_methodA x) c r s))
-- > instance Rebase Ctx SomeM where
-- >   rebase ctx = do
-- >     x' <- rebase (x ctx)
-- >     pure (Ctx x')
-- > -- evalSomeM :: SomeM a -> Ctx -> () -> () -> IO a -- currently omitted
-- > evalSomeM = evalAspect . unSomeM
-- > -- viewSomeM :: SomeM View -> Ctx -> () -> () -> View -- currently omitted
-- > viewSomeM = viewAspect . unSomeM
-- > -- viewSomeMWith :: Reactive -> SomeM View -> Ctx -> () -> () -> View -- currently omitted
-- > viewSomeMWith r = viewAspectWith r . unSomeM
-- > -- viewSomeMStatic :: SomeM View -> Ctx -> () -> () -> View -- currently omitted
-- > viewSomeMStatic = viewAspectStatic . unSomeM
--
mkAspect :: Name -> Q [Dec]
mkAspect aspct = do
  ainfo <- reify aspct
  case ainfo of
    TyConI (NewtypeD _ (Name (OccName nm) _) tyvs _ (RecC _ [(unM,_,ty)]) _) -> do
      case ty of
        AppT (AppT (AppT (AppT (ConT _) (AppT ctx m)) env) state) _ -> do
          cinfo <- reify (extractCtx ctx)
          case cinfo of
            TyConI (DataD [] c _ _ [RecC _ methods] _) -> do
              rebasedCapabilities <- for methods (deriveRebaseCapability unM)
              capabilityMonads <- for methods deriveCapabilityMonad
              rebasedContext <- deriveRebaseContext c
              utils <- makeUtils nm unM
              is <- deriveInstances ctx env state (fmap lowerTVB $ init tyvs)
              pure (rebasedContext : capabilityMonads ++ rebasedCapabilities ++ utils ++ is)
            _ -> error "error in aspect: expected aspect context to be a record of capabilities."
        _ -> error "error in aspect: expected a newtype wrapper around an Aspect type."
    _ ->
      error "error in aspect: expecting newtype wrapper around an Aspect type."
  where
    extractCtx (AppT l _) = extractCtx l
    extractCtx (ConT x) = x

    initTy (AppT l r) = l
    initTy l = l

    lowerTVB (KindedTV nm _) = PlainTV nm
    lowerTVB x = x

    deriveRebaseCapability unM (nm,_,ty) = do
      cinfo <- reify (extractCtx ty)
      case cinfo of
        TyConI (DataD _ _ [] _ _ _) ->
          error "error in mkAspect: context's capabilities must have at least one type variable with a last type variable of kind `* -> *`"
        TyConI (DataD _ c@(Name (OccName capability) _) tyvs _ [RecC con methods] _) ->
          case last tyvs of
            KindedTV tyNm (AppT (AppT ArrowT StarT) StarT) -> do
              ct <- newName "ct"
              c  <- newName "c"
              r  <- newName "r"
              s  <- newName "s"
              nmargs <- for methods $ \(m,_,ty) -> do
                let cnt = argCount 0 ty
                    argCount n (AppT (AppT ArrowT _) r) = argCount (n + 1) r
                    argCount n (ForallT _ _ r) = argCount n r
                    argCount n _ = n
                args <- for [1..cnt] (\_ -> newName "x")
                pure (m,args) -- BindS (VarP nm') (AppE (VarE (mkName $ "prepare" ++ show cnt)) (ParensE (AppE (VarE m) (VarE ct)))))
              pure $ InstanceD Nothing [] (AppT (AppT (ConT (mkName "Rebase")) (ParensT (initTy ty))) (ConT aspct))
                [ FunD (mkName "rebase") 
                    [ Clause [ VarP ct ] 
                        (NormalB $ DoE $
                          [ BindS (VarP c) (VarE $ mkName "ctx")
                          , BindS (VarP r) (VarE $ mkName "ask")
                          , BindS (VarP s) (VarE $ mkName "sref")
                          , NoBindS 
                            (VarE (mkName "pure") `AppE` 
                              foldl (\f (nm,args) -> AppE f $
                                (LamE (map (\a -> VarP a) args) $
                                  VarE (mkName "evalAspect") `AppE`
                                    ParensE (VarE unM `AppE`
                                      ParensE (foldl (\f a -> f `AppE` VarE a) (VarE nm `AppE` VarE ct) args)
                                      ) `AppE` VarE c `AppE` VarE r `AppE` VarE s
                                    )
                                ) (ConE con) nmargs
                            )
                          ]
                        )
                        []
                    ]
                ] 
            _ -> error "error in mkAspect: the last type variable of each of a context's capability must be of kind `* -> *`"
        _ -> error "error in mkAspect: context's capabilities must be parametrically polymorphic single-constructor non-GADT record data type with a last type variable of kind `* -> *`"

    deriveCapabilityMonad (nm,_,ty) = do
      let ctx@(Name (OccName cnm) _) = extractCtx ty
          safeTail [] = []
          safeTail (_:xs) = xs
          vars = safeTail $ init $ extractTys [] ty
            where
              extractTys vs (ConT v) = vs ++ [ConT v]
              extractTys vs (VarT v) = vs ++ [VarT v]
              extractTys vs (AppT l r) = vs ++ extractTys vs l ++ extractTysR r
              extractTys vs (ForallT _ _ ty) = extractTys vs ty

              extractTysR (AppT l r) = [ ParensT (AppT l r) ]
              extractTysR ty = extractTys [] ty

      cinfo <- reify ctx
      case cinfo of
        TyConI (DataD _ _ [] _ _ _) ->
          error "error in mkAspect: context's capabilities must have at least one type variable with a last type variable of kind `* -> *`"
        TyConI (DataD _ c@(Name (OccName capability) _) tyvs _ [RecC con methods] _) ->
          case last tyvs of
            KindedTV tyNm (AppT (AppT ArrowT StarT) StarT) -> do
              let ltyvs = fmap lowerTVB (init tyvs)
              pure $ InstanceD Nothing [] (AppT (foldl AppT (ConT (mkName $ "Monad" ++ cnm)) vars) (ConT aspct))
                [ FunD (mkName $ "with" ++ cnm) 
                    [ Clause [ ] 
                        (NormalB (VarE (mkName ">>=") `AppE` ParensE (VarE (mkName "ctxs" ) `AppE` VarE nm)))
                        []
                    ]
                ] 
            _ -> error "error in mkAspect: the last type variable of each of a context's capability must be of kind `* -> *`"
        _ -> error "error in mkAspect: context's capabilities must be parametrically polymorphic single-constructor non-GADT record data type with a last type variable of kind `* -> *`"
      where
        lowerTVB (KindedTV nm _) = PlainTV nm
        lowerTVB x = x

    deriveRebaseContext ctx = do
      cinfo <- reify ctx
      case cinfo of
        TyConI (DataD _ _ [] _ _ _) ->
          error "error in mkAspect: must have at least one type variable with a last type variable of kind `* -> *`"
        TyConI (DataD _ _ tyvs _ [RecC con methods] _) ->
          case last tyvs of
            KindedTV tyNm (AppT (AppT ArrowT StarT) StarT) -> do
              c <- newName "ctx"
              stmts <- for methods $ \(m@(Name (OccName nm) _),_,ty) -> do
                nm' <- newName nm
                pure (nm',BindS (VarP nm') (VarE (mkName "rebase") `AppE` (ParensE (VarE m `AppE` VarE c))))
              pure $ InstanceD Nothing [] (ConT (mkName "Rebase") `AppT` (ConT ctx) `AppT` (ConT aspct))
                [ FunD (mkName "rebase")
                    [ Clause [ VarP c ]
                        (NormalB $ DoE $
                          ( fmap snd stmts 
                          ) ++ [ NoBindS (VarE (mkName "pure") `AppE` foldl (\f -> AppE f . VarE) (ConE con) (fmap fst stmts)) ]
                          
                        )
                        []
                    ]
                ]
            _ -> error "error in mkAspect: context's last type variable must be of kind `* -> *`"
        _ ->
          error "error in mkAspect: context must be a parametrically polymorphic single-constructor non-GADT record data type with a last type variable of kind `* -> *`"

    makeUtils nm unM = do
      r <- newName "r"
      pure
        [ FunD (mkName $ "eval" ++ nm) 
          [ Clause [] (NormalB $ 
              InfixE (Just $ VarE (mkName "evalAspect")) (VarE (mkName ".")) (Just $ VarE unM)
            ) []
          ]
        , FunD (mkName $ "view" ++ nm) 
          [ Clause [] (NormalB $
              InfixE (Just $ VarE (mkName "viewAspect")) (VarE (mkName ".")) (Just $ VarE unM)
            ) []
          ]
        , FunD (mkName $ "view" ++ nm ++ "With") 
          [ Clause [ VarP r ] (NormalB $
              InfixE (Just $ VarE (mkName "viewAspectWith") `AppE` VarE r) (VarE (mkName ".")) (Just $ VarE unM)
            ) []
          ]
        , FunD (mkName $ "view" ++ nm ++ "Static") 
          [ Clause [] (NormalB $
              InfixE (Just $ VarE (mkName "viewAspectStatic")) (VarE (mkName ".")) (Just $ VarE unM)
            ) []
          ]
        ]

    deriveInstances ctx env state tyvs = do
      let 
        ity1 nm = ConT (mkName nm) `AppT` ParensT (foldl (\f (PlainTV nm) -> f `AppT` VarT nm) (ConT aspct) tyvs)
        ity2 nm l = ConT (mkName nm) `AppT` l `AppT` ParensT (foldl (\f (PlainTV nm) -> f `AppT` VarT nm) (ConT aspct) tyvs)
      pure
        [ StandaloneDerivD Nothing [] (ity1 "Functor") 
        , StandaloneDerivD Nothing [] (ity1 "Applicative")
        , StandaloneDerivD Nothing [] (ity1 "Monad")
        , StandaloneDerivD Nothing [] (ity1 "MonadIO")
        , StandaloneDerivD Nothing [] (ity2 "MonadReader" (ParensT env))
        , StandaloneDerivD Nothing [] (ity2 "MonadContext" (ParensT $ ctx `AppT` ConT aspct))
        , StandaloneDerivD Nothing [] (ity2 "MonadSRef" (ParensT state))
        , StandaloneDerivD Nothing [] (ity1 "MonadThrow")
        , StandaloneDerivD Nothing [] (ity1 "MonadCatch")
        , StandaloneDerivD Nothing [] (ity1 "MonadMask")
        , StandaloneDerivD Nothing [] (ity1 "MonadFix")
        ]

