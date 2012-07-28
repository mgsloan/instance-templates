{-# LANGUAGE
    ConstraintKinds
  , DeriveDataTypeable
  , EmptyDataDecls
  , KindSignatures
  , PatternGuards
  , QuasiQuotes
  , ScopedTypeVariables
  , TemplateHaskell
  , TupleSections
  , ViewPatterns
  #-}

module Language.Haskell.InstanceTemplates 
  ( mkTemplate, instantiate, template
  , Inherit, Instance

  -- * Things that generated code depends on
  , Template(..), TInstance(..), TemplateOutput(..)
  , processHead, processInstance, processHiding
  ) where

import Control.Applicative   ( (<$>) )
import Control.Arrow         ( (***), (&&&) )
import Control.Monad         ( zipWithM )
import Control.Monad.State   ( State, evalState, get, put )

import Data.Char             ( isSpace )
import Data.Function         ( on )
import Data.Generics         ( Data, listify, everywhere, something, gmapM )
import Data.Generics.Aliases ( extT, extQ, extM )
import Data.Typeable         ( Typeable(..) )
import Data.List             ( find, sortBy )
import Data.Maybe            ( fromJust, catMaybes, isJust )
import Data.Ord              ( comparing )
import qualified Data.Map as M
import GHC.Exts              ( Constraint )

import Debug.Trace ( trace )

import Language.Haskell.TH
import Language.Haskell.TH.Instances  ( )
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift       ( deriveLift )

-- Just like the InstanceD constructor, but not inside a large sum-type.
data TInstance = TInstance Cxt Type [Dec]
  deriving (Eq, Show, Data, Typeable)

$(deriveLift ''TInstance)

instanceTy   :: TInstance -> Type
instanceDecl :: TInstance -> Dec

instanceTy   (TInstance _ t _ ) = t
instanceDecl (TInstance c t ds) = InstanceD c t ds

data TemplateOutput = TemplateOutput Type [TInstance]
  deriving (Eq, Show)

class Template a where
  invokeTemplate :: a -> Type -> [Dec] -> TemplateOutput


processHead :: Type -> ([Pred], [Type])
processHead typ = (l, tail $ tyUnApp r)
 where
  (l, r) = case typ of
    (ForallT _ ps t) -> (ps, t)
    t                -> ([], t)

processInstance :: [Type] -> TInstance -> Maybe TInstance
processInstance hides (TInstance ctx typ ds)
  = Just $ TInstance (process_context ctx) typ ds
 where
  process_context = filter not_satisfied
   where
    --TODO: check if they're actually "satisfied"?
    not_satisfied = isJust . something (const Nothing `extQ` just_var)
    just_var (VarT _) = Just True
    just_var _ = Nothing

processHiding :: [Dec] -> [Type]
processHiding = debug . concatMap helper
 where
  helper (InstanceD _ (AppT (ConT ni) (AppT (ConT nm) t)) _)
    | nameBase ni == "Hide" && nameBase nm == "Instance"
    = tyFlatten t
  helper _ = []

debug :: Show a => a -> a
debug x = trace (show x) x

-- Yields all of the names that the declaration introduces.
-- Note that duplication might occur from data declarations.
decNames :: Dec -> [Name]
decNames (FunD         n _            ) = [n]
decNames (TySynD       n _ _          ) = [n]
decNames (FamilyD      _ n _ _        ) = [n]
decNames (NewtypeD     _ n _ c _      ) = n : conNames c
decNames (DataD        _ n _ c _      ) = n : concatMap conNames c
decNames (ClassD       _ n _ _ d      ) = n : concatMap decNames d
--It's weird to have this, but necessary for my use here...
decNames (InstanceD    _ _ d          ) = concatMap decNames d
decNames (DataInstD    _ _ _ c _      ) = concatMap conNames c
decNames (NewtypeInstD _ _ _ c _      ) = conNames c
decNames (ValD         p _ _          ) = patNames p
decNames (SigD         n _            ) = [n]
decNames (ForeignD (ImportF _ _ _ n _)) = [n]
-- Handles ForeignD (ExportF ...), PragmaD, TySynInstD
decNames _                              = []

conNames :: Con -> [Name]
conNames (NormalC n _  ) = [n]
conNames (RecC    n r  ) = n : map (\(x, _, _) -> x) r
conNames (InfixC  _ n _) = [n]
conNames (ForallC _ _ c) = conNames c

patNames :: Pat -> [Name]
patNames (VarP        n     ) = [n]
patNames (TupP        p     ) = concatMap patNames p
patNames (UnboxedTupP p     ) = concatMap patNames p
patNames (InfixP      l _ r ) = patNames l ++ patNames r
patNames (UInfixP     l _ r ) = patNames l ++ patNames r
patNames (ParensP     p     ) = patNames p
patNames (BangP       p     ) = patNames p
patNames (TildeP      p     ) = patNames p
patNames (AsP         n p   ) = n : patNames p
patNames (RecP        _ f   ) = concatMap (patNames . snd) f
patNames (ListP       p     ) = concatMap patNames p
patNames (SigP        p _   ) = patNames p
patNames (ViewP       _ p   ) = patNames p
patNames _                    = []

-- Convenience function for use with TH AST quoters

template :: Template a => a -> TypeQ -> DecsQ -> Q TemplateOutput
template tparam qty qds = do
  ds <- qds
  ty <- qty
  let ty' = everywhere (id `extT` deUniqueTVs) ty
      ds' = everywhere (id `extT` deUniqueTVs)
          $ deUniqueNames (concatMap decNames ds) ds
  return $ invokeTemplate tparam ty' ds'

deUniqueName :: Name -> Name
deUniqueName (Name o (NameU _)) = Name o NameS
deUniqueName n                  = n

deUniqueTVs :: Type -> Type
deUniqueTVs (VarT n) = VarT $ deUniqueName n
deUniqueTVs t        = t

deUniqueNames :: Data a => [Name] -> a -> a
deUniqueNames names = everywhere (id `extT` subst)
 where
  subst n | n `elem` names = deUniqueName n
          | otherwise = n

-- TODO: Read up on the specifics of OverlappingInstance" and figure out how
-- they fit into this picture.  Force "hiding" declarations?  Certainly not
-- going to handle it explicitly for now.

-- TODO: do reify checks for existing instances.

-- TODO: derive Typeable for anything that needs it.

-- Instantiates a set of instance templates
instantiate :: [Q TemplateOutput] -> DecsQ
instantiate qdos
  = ( concatMap (\(TemplateOutput _ xs) -> map instanceDecl xs)
    . subsume
    . sortBy (comparing (\(TemplateOutput _ xs) -> length xs))
    ) <$> sequence qdos
 where
  subsume (cur@(TemplateOutput dhead ds):xs) = cur : map process xs
   where
    process unmodified@(TemplateOutput dhead' ds') =
      case (length results, length just_results) of
        (lr, ljr)

         | lr == ljr && length ds' /= length ds
          -> warn ( pprint dhead
                 ++ " is suppressing the instances generated by "
                 ++ pprint dhead'
                 ++ ":\n"
                 ++ err_instances
                  )
           . TemplateOutput dhead'
           $ filter (`notElem` map fst just_results) ds'

         | lr == ljr
          -> error
           $ "The instances generated by "
          ++ err_heads
          ++ " fully overlap, both of them providing the following instances:"
          ++ err_instances

         | ljr == 0 -> unmodified

         | otherwise
          -> error
           $ "Some overlap between the instances generated by "
          ++ err_heads
          ++ ", but one is not a proper subset of the other.\n"
          ++ "The following instances overlap:\n"
          ++ err_instances

     where
      just_results = catMaybes results
      results = map (\x -> (x,) <$> find (instanceOverlaps x) ds) ds'
      instanceOverlaps = headOverlaps `on` instanceTy

      err_heads = pprint dhead ++ " and " ++ pprint dhead'
      err_instances = unlines . map (show . ppr . instanceTy . snd)
                    $ just_results
  -- Handles [x] and []
  subsume x = x

  warn msg = trace ("WARNING: " ++ msg)


-- This overlaps stuff is probably wrong, purely derived from my non-formal
-- intuition for instance overlap.

-- NOTE: this operating even approximately properly depends on the names being
-- fully qualified.


{- Wrong due to var equivalence

tyOverlaps :: Type -> Type -> Bool
tyOverlaps l r = overlapBool $ tyOverlap l r

data Overlap
  = NonOverlapping
  | VarEquivalent
  | Overlapping

overlapBool :: Overlap -> Bool
overlapBool NonOverlapping = False
overlapBool _ = True

tyOverlap :: Type -> Type -> Overlap
tyOverlap (ForallT _ _ _) _ = error "Forall not allowed in instance declarations!"
tyOverlap _ (ForallT _ _ _) = error "Forall not allowed in instance declarations!"
tyOverlap (SigT l _) r = tyOverlap l r
tyOverlap l (SigT r _) = tyOverlap l r
tyOverlap (VarT _) (VarT _) = VarEquivalent
tyOverlap (AppT l r) (AppT l' r') = case (tyOverlap l l', tyOverlap r r') of
  (NonOverlapping, _) -> NonOverlapping
  (_, NonOverlapping) -> NonOverlapping
  (   Overlapping, _) ->    Overlapping
  (_,    Overlapping) ->    Overlapping
  _                   -> VarEquivalent
tyOverlap x y
  | x == y    =    Overlapping
  | otherwise = NonOverlapping
tyOverlap
-}

headOverlaps :: Type -> Type -> Bool
headOverlaps = (==) `on` headCannon

headCannon :: Type -> Type
headCannon t = evalState (helper t) (vars, M.empty)
 where
  helper :: Type -> State ([Name], M.Map Name Name) Type
  helper (VarT n) = do
    ((n':ns), m) <- get
    case M.lookup n m of
      Just n'' -> return $ VarT n''
      Nothing  -> do
        put (ns, M.insert n n' m)
        return $ VarT n'
  helper (ForallT _ _ _) = fail "Forall not allowed in instance heads!"
  helper ty = gmapM (return `extM` helper) ty

  vars = [ mkName $ filter (not . isSpace) [a, b, c]
         | c <- az, b <- az, a <- tail az ]
   where
    az = ' ' : ['a'..'z']

{-
This was removed because making instances of stuff like:

    Not in scope: type constructor or class `GHC.Types.Int'

-- TODO: Invoke NameQ / NameS when package / module info are empty? Necessary?
  $ NameG DataName (mkPkgName $ tyConPackage tc) 
                   (mkModName $ tyConModule  tc)
 -}

data Instance (a :: Constraint)

class Inherit a where

class Hide a where

-- TODO list
--
-- * Make the template generate template invocations instead of generating
--   instances.  Have a special invocation for directly wrapping classes?
--
-- * Handle type / data families
--
-- * Add the ability to hide generated instances.
--
-- * Check provided definitions
--
-- * Handle default definitions
--
-- * Only put necessary definitions in where statements
--
-- * Handle whereless parameters in a different fashion - have the others just
--   use the method from the other class.
--
-- * Do constraint strengthening by allowing signatures on functions.
--
-- * Catch kind errors at instance-build time (better yet infer them!)
--   http://hackage.haskell.org/package/th-kinds


-- | Takes a code representation of a template and generate an instance which,
--   when invoked, builds a particular instance of the deriving class.
mkTemplate :: [Dec] -> DecsQ
mkTemplate (top@(ClassD inp_cxt inp_name inp_tvs inp_fds inp_ds) : insts)
  | not (null inp_fds)
  = error "Functional dependencies not allowed in instance templates."

--TODO: It /might/ be nice to be able to have instance templates that generate
--      no instances..
  | null insts
  = error "Instance template must generate some instances."

  | otherwise = do
      dd <- template_decl
      return [data_decl, dd]
 where
  -- Top-level names - things that should be de-uniqued.
  top_names = concatMap decNames (top : insts)

  -- For prototyping purposes, this is OK.
  head_name = mkName $ nameBase inp_name ++ "_T"

  data_decl = DataD [] head_name inp_tvs [NormalC head_name []] []

  {- TODO: re-enable once GHC 7.6 is released
  type_decl = TySynD inp_name inp_tvs . mkTupleT
            $ [ty | Just ty <- map predToType inp_cxt]
           ++ [ty | (InstanceD _ ty _) <- insts]
  -}

  -- TODO: resolve kind arities by reifying the constraints and doing kind
  -- inference.  For now * is assumed unless an explicit sig is given.
  template_decl :: DecQ
  template_decl = do
    -- References to local variables
    let n_typ   = mkName "typ"
        n_ctx   = mkName "ctx"
        n_decls = mkName "decls"
        n_hides = mkName "hides"
  
    -- Converts the TyVarBndrs to unqualified Name.
    let tv_names = map (deUniqueName . tvName) inp_tvs

        tvars = map VarT tv_names

    -- The head of the instance of Template.
        inst_head = AppT (ConT $ mkName "Template")
                  . mkAppsT $ ConT head_name : tvars

    -- The head of the generated TemplateOutput.
        gen_head = mkAppsT $ ConT gen_name : tvars
        gen_name = mkName $ nameBase inp_name

        tv_len = litE . IntegerL . fromIntegral $ length inp_tvs

    -- Expression that checks that the type supplied to the instance template
    -- has the right number of parameters.
    tv_extract <-
      [e|
      case processHead $( varE n_typ ) of
        (x, l) | length l == $( tv_len ) -> (x, l)
               | otherwise 
               -> error $
                    $( litE . StringL $ nameBase inp_name )
                    ++ " expects " ++ show ($( tv_len ) :: Int)
                    ++ " class parameter(s)"
      |]

    -- Locals to represent the types provided to the different type parameters.
    let n_tvs = map (mkName . ("tv_" ++) . nameBase) tv_names

    -- Where-declaration for extracting the type variables into these locals.
        tvs_dec = ValD (TupP [VarP n_ctx, ListP $ map VarP n_tvs])
                       (NormalB tv_extract)
                       []

    -- Substitutions that rewrite the type variables to refer to these locals.
    -- This substitution is done after lifting the declarations, so they need
    -- to match on the lifted version of the type variables.
    tsubs <- M.fromList <$> zipWithM (\t n -> (, VarE n) <$> lift t) tvars n_tvs

    --TODO: figure out what to use for the parameter representation.

    -- Builds the instance declarations.
    (params, inst_ds) <- unzip <$> mapM mk_inst insts

    -- Lifts the instance declarations to TH code that builds them
    let gen_insts = map ( \x -> [e| processInstance $( varE n_hides ) $( lift x ) |] )
                  $ concat inst_ds

    -- An expression for the yielded TemplateOutput.
    expr <- [e| TemplateOutput
                  $( lift $ deUniqueNames top_names gen_head )
                  (catMaybes $( listE gen_insts ))
              |]

    -- Lifted version of the placeholder in the where declarations of the methods.
    decls_sub <- ListE . (:[]) <$> lift placeholder

    -- Rewritten version of the expression that generates TemplateOutput.
    let expr' = everywhere (id `extT` trans) expr

    -- Substitute in references to the declarations passed into invokeTemplate.
        trans x | x == decls_sub = VarE n_decls
    -- Substitute references to the type parameters passed into invokeTemplate.
                | Just v <- M.lookup x tsubs = v
                | otherwise = x

    -- Where declaration that computes the parameter to processInstance, which
    -- provides the instance-head types that should be hidden.
    hides_dec <- valD (varP n_hides)
                      (normalB . appE [e| processHiding |] $ varE n_decls)
                      []

    let clauz  = Clause pats (NormalB expr') wheres
        pats   = [WildP, VarP n_typ, VarP n_decls]
        wheres = [tvs_dec, hides_dec]

    return $ InstanceD [] inst_head [ FunD (mkName "invokeTemplate") [clauz] ]

  mk_inst (InstanceD ctx typ decs) = do
    (everywhere (id `extT` deUniqueTVs) . deUniqueNames top_names)
     <$> case typ of
        (AppT (ConT ni) (AppT (ConT nm) typ'))
          | nameBase ni == "Inherit" && nameBase nm == "Instance"
          -> case (null ctx, null decs) of
              (False, False) -> fail
                "Inheriting instances cannot have constraints or declarations."
              (False, True) -> fail
                "Inheriting instances cannot have constraints."
              (True, False) -> fail
                "Inheriting instances cannot have declarations."
              (True, True) -> do
                let typs = tyFlatten $ everywhere (id `extT` deUniqueName) typ'
-- import Language.Haskell.TH.ExpandSyns ( expandSyns )
--                typs <- tyFlatten <$> expandSyns de_uniqued
                (concat *** id) . unzip <$> mapM typ_inherit typs

        -- Standard case
        _ -> return ([], [TInstance (inp_cxt ++ ctx) typ $ map mk_dec decs])
   where
    -- TODO: less partiality
    typ_inherit
      = fmap mk_inherit . firstF reify
      . (fromJust . tyConName . head &&& tail) . tyUnApp
    -- TODO: check if the deriving class constraints are a superset
    mk_inherit (ClassI (ClassD _ n _ _ ds) _, ips)
      = (concatMap decNames ds, TInstance ctx (mkAppsT $ ConT n : ips) ds')
     where
      ds' = concatMap sig_dec ds
      sig_dec (SigD fn _)
        = [mk_dec $ ValD (VarP fn) (NormalB . VarE . mkName $ nameBase fn) []]
      sig_dec _ = []
      {-
      ds' = everywhere (id `extT` process) ds
      process t | Just t' <- M.lookup t t_map = t'
                | otherwise                   = t
      t_map = M.fromList $ zip (map (VarT . tvName) tvs) ips
      -}
    mk_inherit (i, _) 
      = error $ "Can only inherit from classes. Got:\n" ++ pprint i

  mk_inst _ = error
    "Instance Templates can only contain parameter classes and instances."

  mk_dec dec = case dec of
    (ValD (VarP n) b ds) -> build_dec proxy n (ValD (VarP proxy) b ds)
    (FunD       n    cs) -> build_dec proxy n (FunD       proxy    cs)
--    ()
--    (TySynInstD )
--    (DataInstD )
--    (NewtypeInstD )
    _ -> error
       $ "Cannot handle the following in an instance declaration: "
      ++ pprint dec
   where
    proxy = available "def"

  build_dec proxy name renamed
    = ValD (VarP name)
           (NormalB . LetE [renamed] $ VarE proxy)
           [placeholder]

  placeholder = ValD (VarP name) (NormalB $ VarE name) []
    where name = available "placeholder"

  --TODO: remove, as probably unnecessary
  available prefix
    = fromJust
    . find (`notElem` (allNames top ++ allNames insts))
    $ map (mkName . (prefix ++) . empty0) ([0..] :: [Int])

  empty0 0 = ""
  empty0 n = show n

mkTemplate _ = error "First declaration passed to mkTemplate must be a class."


-- Utils for mkTemplate

-- Note: Due to lack of TH support for constraint kinds, equality
-- constraints aren't supported.
predToType :: Pred -> Maybe Type
predToType (ClassP n ts) = Just $ foldl AppT (ConT n) ts
predToType _ = Nothing

mkTupleT :: [Type] -> Type
mkTupleT xs = foldl AppT (TupleT (length xs)) xs

mkAppsT :: [Type] -> Type
mkAppsT = foldl1 AppT

tvName :: TyVarBndr -> Name
tvName (PlainTV  n  )   = n
tvName (KindedTV n _) = n

tvArity :: TyVarBndr -> Int
tvArity (PlainTV  _  ) = 1
tvArity (KindedTV _ k) = kindArity k

kindArity :: Kind -> Int
kindArity StarK = 1
kindArity (ArrowK _ r) = 1 + kindArity r

tyFlatten :: Type -> [Type]
tyFlatten t = case tyUnApp t of
 (TupleT _ : xs) -> concatMap tyFlatten xs
 _               -> [t]

tyUnApp :: Type -> [Type]
tyUnApp = reverse . helper
 where
  helper (AppT l r) = r : helper l
  helper x          = [x]

tyConName :: Type -> Maybe Name
tyConName (ConT n) = Just n
tyConName _ = Nothing

allNames :: Data d => d -> [Name]
allNames = listify (const True :: Name -> Bool)

firstF :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
firstF f (x, y) = (,y) <$> f x