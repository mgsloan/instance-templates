{-# LANGUAGE
    ConstraintKinds
  , DeriveDataTypeable
  , EmptyDataDecls
  , KindSignatures
  , PatternGuards
  , QuasiQuotes
  , TemplateHaskell
  , TupleSections
  , ViewPatterns
  #-}

module Language.Haskell.InstanceTemplates 
  ( mkTemplate, instantiate, template

  -- * Things that generated code depends on
  , Template(..), TInstance(..), TemplateOutput(..), processHead
  ) where

import Control.Applicative   ( (<$>) )

import Data.Function         ( on )
import Data.Generics         ( Data, listify, everywhere )
import Data.Generics.Aliases ( extT )
import Data.Typeable         ( Typeable(..) )
import Data.List             ( find, sortBy )
import Data.Maybe            ( fromJust, catMaybes )
import Data.Ord              ( comparing )
import qualified Data.Map as M

import Debug.Trace ( trace )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift ( deriveLift )

-- TODO: replace with an orphans package, with the code from:
-- https://github.com/mgsloan/quasi-extras/blob/5ab894f9868324698f2c9d6d8b9acfc79bf76a9c/reifyQ.hs
import Language.Haskell.Meta ()

-- Just like the InstanceD constructor, but not inside a large sum-type.
data TInstance = TInstance Cxt Type [Dec]
  deriving (Eq, Show, Data, Typeable)

$(deriveLift ''TInstance)

instanceTy   :: TInstance -> Type
instanceDecl :: TInstance -> Dec

instanceTy (TInstance _ t _) = t
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
-- Handles InstanceD, SigD, ForeignD (ExportF ...), PragmaD, TySynInstD
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
patNames (SigP        p t   ) = patNames p
patNames (ViewP       e p   ) = patNames p
patNames _                    = []

{- TODO: remove
infoName :: Info -> Name
infoName (ClassI     d _    ) = head $ decNames d
infoName (ClassOpI   n _ _ _) = n
infoName (TyConI     d      ) = head $ decNames d
infoName (FamilyI    d _    ) = head $ decNames d
infoName (PrimTyConI n _ _  ) = n
infoName (DataConI   n _ _ _) = n
infoName (VarI       n _ _ _) = n
infoName (TyVarI     n _    ) = n
-}

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
      instanceOverlaps = tyOverlaps `on` instanceTy

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

{-
This was removed because making instances of stuff like:

    Not in scope: type constructor or class `GHC.Types.Int'

-- TODO: Invoke NameQ / NameS when package / module info are empty? Necessary?
  $ NameG DataName (mkPkgName $ tyConPackage tc) 
                   (mkModName $ tyConModule  tc)
 -}

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
-- * Handle inheriting methods from whereless instances.  The mechanism here
--   be built into the code that checks the provided declarations, and should
--   suppress providing definitions for those methods in the whereless
--   instances.
--
-- * Consider how the generated type synonym works if superclass constraints
--   are allowed on the instances...
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
  -- TODO: re-enable once GHC 7.6 is released
  type_decl = TySynD inp_name inp_tvs . mkTupleT
            $ [ty | Just ty <- map predToType inp_cxt]
           ++ [ty | (InstanceD _ ty _) <- insts]

  -- TODO: make sure this doesn't reify to anything, etc.
  -- For prototyping purposes, this is OK.
  head_name = mkName $ nameBase inp_name ++ "_Template"

  tvars = map (VarT . deUniqueName . tvName) inp_tvs

  data_decl = DataD [] head_name inp_tvs [NormalC head_name []] []

  inst_head = AppT (ConT $ mkName "Template")
            . foldl1 AppT
            $ ConT head_name : tvars

  out_head = foldl1 AppT
           $ ConT (mkName $ nameBase inp_name) : tvars
  
  -- TODO: resolve kind arities by reifying the constraints and doing kind
  -- inference.  For now * is assumed unless an explicit sig is given.
  template_decl :: DecQ
  template_decl = do
    expr <- [e| TemplateOutput
                  $( lift $ deUniqueNames top_names out_head )
                  $( listE $ map (lift . mk_inst) insts )
              |]
    esub <- ListE . (:[]) <$> lift placeholder
    tsubs <- M.fromList <$> mapM (\(t, e) -> (, e) <$> lift t) unlifted_subs

    let expr' = everywhere (id `extT` trans) expr
        trans x | x == esub = VarE n_decls
                | Just v <- M.lookup x tsubs = v
                | otherwise = x

    tv_dec <- valD (tupP [varP n_ctx, listP $ map return tvpats]) 
                   (normalB tv_extract)
                   []
    return
      $ InstanceD [] inst_head
      [ FunD (mkName "invokeTemplate") [Clause pats (NormalB expr') [tv_dec]] ]
   where
    pats = [WildP, VarP n_typ, VarP n_decls]
    n_typ   = mkName "typ"
    n_ctx   = mkName "ctx"
    n_decls = mkName "decls"
    tv_extract = [e|case processHead $(varE n_typ) of
                      (x, l) | length l == $(tv_len) -> (x, l)
                             | otherwise -> error $ $(tv_error)
                   |]
    tv_len = litE . IntegerL . fromIntegral $ length inp_tvs
    tv_error = [e| $(litE . StringL $ nameBase inp_name)
                ++ " expects " ++ show $(tv_len) ++ " class parameter(s)" |]
    (unlifted_subs, tvpats) = unzip $ map (mk_tv . nameBase . tvName) inp_tvs

  mk_tv n = ((VarT $ mkName n, VarE tv), VarP tv)
   where
    tv = mkName $ "tv_" ++ n

  mk_inst (InstanceD ctx typ decs)
    = everywhere (id `extT` deUniqueTVs)
    . deUniqueNames top_names
    $ TInstance (inp_cxt ++ ctx) typ $ map mk_dec decs
  mk_inst _
    = error "Instance Templates can only contain parameters and instances."

  placeholder = ValD (VarP name) (NormalB $ VarE name) []
    where name = available "placeholder"

  mk_dec dec = case dec of
    (ValD _ _ _) -> build_dec proxy name renamed
    (FunD _ _  ) -> build_dec proxy name renamed
--    ()
--    (TySynInstD )
--    (DataInstD )
--    (NewtypeInstD )
    _ -> error
       $ "Cannot handle the following in an instance declaration: "
      ++ pprint dec
   where
    proxy   = available "def"
    name    = dec_name dec
    renamed = rename_dec dec proxy

  build_dec proxy name renamed
    = ValD (VarP name)
           (NormalB . LetE [renamed] $ VarE proxy)
           [placeholder]

  -- TODO: do we need to handle more types of patterns??
  dec_name (ValD (VarP n) _ _) = n
  dec_name (FunD n _) = n
  dec_name x = error $ "Unsupported type of declaration: " ++ show x

  rename_dec (ValD _ b ds) n' = ValD (VarP n') b ds
  rename_dec (FunD _ cs) n' = FunD n' cs
  rename_dec x _ = error $ "Unsupported type of declaration: " ++ show x

  --TODO: remove, as probably unnecessary
  available prefix
    = fromJust
    . find (`notElem` (allNames top ++ allNames insts))
    $ map (mkName . (prefix ++) . empty0) ([0..] :: [Int])

  top_names = concatMap decNames (top : insts)

  empty0 0 = ""
  empty0 n = show n

mkTemplate _ = error "First declaration passed to mkTemplate must be a class."

debug x = trace (show x) x

-- Utils for buildTemplate

-- Note: Due to lack of TH support for constraint kinds, equality
-- constraints aren't supported.
predToType :: Pred -> Maybe Type
predToType (ClassP n ts) = Just $ foldl AppT (ConT n) ts
predToType _ = Nothing

mkTupleT :: [Type] -> Type
mkTupleT xs = foldl AppT (TupleT (length xs)) xs

tvName :: TyVarBndr -> Name
tvName (PlainTV  n  )   = n
tvName (KindedTV n _) = n

tvArity :: TyVarBndr -> Int
tvArity (PlainTV  _  ) = 1
tvArity (KindedTV _ k) = kindArity k

kindArity :: Kind -> Int
kindArity StarK = 1
kindArity (ArrowK _ r) = 1 + kindArity r

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