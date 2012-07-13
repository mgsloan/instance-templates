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
  ( Template(..), instantiate, instance', mkTV, deriving'

  -- Things the generated code depends on

  , TInstance(..), TemplateOutput(..), tvType

  , Typeable
  , TV_a , TV_b , TV_c
  , TV_a1, TV_b1, TV_c1
  , TV_a2, TV_b2, TV_c2
  , TV_a3, TV_b3, TV_c3
  ) where

import Control.Arrow         ( first, second, (***) )
import Control.Applicative   ( (<$>) )

import Data.Char             ( isSpace )
import Data.Function         ( on )
import Data.Generics         ( Data, listify, everywhere )
import Data.Generics.Aliases ( extT )
import Data.Typeable         ( Typeable(..), TypeRep, TyCon, typeRepTyCon, typeRepArgs, tyConName ) -- , tyConPackage, tyConModule )
import Data.List             ( find, isPrefixOf, inits, tails, sortBy )
import Data.Maybe            ( fromJust, catMaybes )
import Data.Ord              ( comparing )
import qualified Data.Map as M

import Debug.Trace ( trace )

import Language.Haskell.TH
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift ( deriveLift )
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Meta as Exts


-- Just like the InstanceD constructor, but not inside a large sum-type.
data TInstance = TInstance Cxt Type [Dec]
  deriving (Eq, Show)

$(deriveLift ''TInstance)

instanceTy   :: TInstance -> Type
instanceDecl :: TInstance -> Dec

instanceTy (TInstance _ t _) = t
instanceDecl (TInstance c t ds) = InstanceD c t ds

data TemplateOutput = TemplateOutput Type [TInstance]
  deriving (Eq, Show)

class Template a where
  invokeTemplate :: a -> Cxt -> [Dec] -> TemplateOutput

-- Yields all of the names that the declaration introduces.
-- Note that duplication might occur from data declarations.
decNames :: Dec -> [Name]
decNames (FunD         n _            ) = [n]
decNames (DataD        _ n _ c _      ) = n : concatMap conNames c
decNames (NewtypeD     _ n _ c _      ) = n : conNames c
decNames (TySynD       n _ _          ) = [n]
decNames (ClassD       _ n _ _ _      ) = [n]
decNames (FamilyD      _ n _ _        ) = [n]
decNames (DataInstD    _ _ _ c _      ) = concatMap conNames c
decNames (NewtypeInstD _ _ _ c _      ) = conNames c
decNames (ValD     (VarP n) _ _       ) = [n]
decNames (ForeignD (ImportF _ _ _ n _)) = [n]
-- Handles InstanceD, SigD, ForeignD (ExportF ...), PragmaD, TySynInstD
decNames _                              = []

conNames :: Con -> [Name]
conNames (NormalC n _  ) = [n]
conNames (RecC    n r  ) = n : map (\(x, _, _) -> x) r
conNames (InfixC  _ n _) = [n]
conNames (ForallC _ _ c) = conNames c


instance' :: Template a => TypeQ -> a -> DecsQ -> Q TemplateOutput
instance' qctx tparam qds = do
  runIO $ print "a"
  ds <- qds
  runIO $ print "b"
  ctx <- qctx
  runIO $ print "c"
  let names    = concatMap decNames ds
      rewrites = M.fromList . zip names $ map rewrite names
      subst n  = maybe n id $ M.lookup n rewrites
      ctx'     = case ctx of
        (ForallT [] ps _) -> ps
        (ConT n) | nameBase n == "()" -> []
        _ -> error "Instance context must be in the form of a forall or ()."
  return $ invokeTemplate tparam
  -- TODO: might need to also process ClassP, to support Constraint kinds.
    (everywhere (id `extT` tvType') ctx')
    (everywhere (id `extT` subst) ds)
 where
  rewrite (Name o (NameU _)) = Name o NameS
  rewrite n = n

-- TODO: Read up on the specifics of OverlappingInstance" and figure out how
-- they fit into this picture.  Force "hiding" declarations?  Certainly not
-- going to handle it explicitly for now.

-- TODO: do reify checks for existing instances.

-- Instantiates a set of instance templates
instantiate :: [Q TemplateOutput] -> DecsQ
instantiate qdos = do
  dos <- sequence qdos
  return
    . concatMap (\(TemplateOutput _ xs) -> map instanceDecl xs)
    . subsume
    . sortBy (comparing (\(TemplateOutput _ xs) -> length xs))
    $ dos
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
  _                   -> VarEquivalent  -- All that's left
tyOverlap x y
  | x == y    =    Overlapping
  | otherwise = NonOverlapping

tvType :: Typeable a => a -> Type
tvType = everywhere (id `extT` tvType') . thType

tvType' :: Type -> Type
tvType' (ConT n)
  | "TV_" `isPrefixOf` nameBase n = VarT . mkName . drop 3 $ nameBase n
  | otherwise = ConT n
tvType' t = t


thType :: Typeable a => a -> Type
thType = thTypeRep . typeOf

thTypeRep :: TypeRep -> Type
thTypeRep tr = foldl AppT (thTyCon (typeRepTyCon tr))
             $ map thTypeRep (typeRepArgs tr)

debug x = trace (show x) x

thTyCon :: TyCon -> Type
thTyCon tc = ConT $ Name (mkOccName $ tyConName tc) NameS
{-
This was removed because making instances of stuff like:

    Not in scope: type constructor or class `GHC.Types.Int'

-- TODO: Invoke NameQ / NameS when package / module info are empty? Necessary?
  $ NameG DataName (mkPkgName . debug $ tyConPackage tc) 
                   (mkModName . debug $ tyConModule  tc)
 -}

-- Builds a dummy data-type representing polymorphic variables.
mkTV :: Int -> String -> DecsQ
mkTV a n = do
  tvs <- mapM (const $ newName "tv") [1..a]
  let tvs' = map PlainTV tvs
  return [ DataD [] (mkName $ "TV_" ++ n) tvs' [] [mkName "Typeable"] ]

-- Curse the stage restriction!
data TV_a        deriving Typeable
data TV_b        deriving Typeable
data TV_c        deriving Typeable
data TV_a1 a     deriving Typeable
data TV_b1 a     deriving Typeable
data TV_c1 a     deriving Typeable
data TV_a2 a b   deriving Typeable
data TV_b2 a b   deriving Typeable
data TV_c2 a b   deriving Typeable
data TV_a3 a b c deriving Typeable
data TV_b3 a b c deriving Typeable
data TV_c3 a b c deriving Typeable

-- The result of parsing.

data TemplateRep = TemplateRep Dec [Dec]
  deriving (Eq, Show, Data, Typeable)

deriving' :: QuasiQuoter
deriving' = QuasiQuoter undefined undefined undefined
                        (buildTemplate . parseTemplate)

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
-- * Consider how the generated type synonym works if superclass constraints
--   are allowed on the instances...

-- | Takes a code representation of a template and generate an instance which,
--   when invoked, builds a particular instance of the deriving class.
buildTemplate :: TemplateRep -> DecsQ
buildTemplate
 rep@(TemplateRep (ClassD inp_cxt inp_name inp_tvs inp_fds inp_ds) insts)
  | not (null inp_fds)
  = error "Functional dependencies not allowed in instance templates."

--TODO: It /might/ be nice to be able to have instance templates that generate
--      no instances..
  | null insts
  = error "Instance template must generate some instances."

  | otherwise = do
      dd <- template_decl
      return [type_decl, data_decl, dd]
 where
  type_decl = TySynD inp_name inp_tvs . mkTupleT
            $ [ty | Just ty <- map predToType inp_cxt]
           ++ [ty | (InstanceD _ ty _) <- insts]


  -- TODO: make sure this doesn't reify to anything, etc.
  -- For prototyping purposes, this is OK.
  head_name = mkName $ nameBase inp_name ++ "_Template"

  tvars = map (VarT . tvName) inp_tvs

  data_decl = DataD [] head_name inp_tvs [] []
--    where con = NormalC head_name $ map (NotStrict,) tvars

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
                  $( lift out_head )
                  $( listE $ map (lift . mk_inst) insts )
              |]
    esub <- ListE . (:[]) <$> lift placeholder
    tsubs <- M.fromList <$> mapM (\(t, e) -> (, e) <$> lift t) unlifted_subs
    let expr' = everywhere (id `extT` trans) expr
        trans x | x == esub = VarE decls_name
                | Just v <- M.lookup x tsubs = v
                | otherwise = x
    return
      $ InstanceD (map mk_typeable inp_tvs) inst_head
      [ FunD (mkName "invokeTemplate") [Clause pats (NormalB expr') wheres] ]
   where
    pats = [WildP, VarP cxt_name, VarP decls_name]
    cxt_name   = mkName "cxt"
    decls_name = mkName "decls"
    ty_prefix = available "ty_"
    (unlifted_subs, wheres) = unzip $ map (mk_where . nameBase . tvName) inp_tvs

  mk_where n = ((VarT $ mkName n, VarE tv), ValD (VarP tv) (NormalB expr) [])
   where
    tv = mkName $ "tv_" ++ n
    expr = AppE (VarE $ mkName "tvType")
         . SigE (VarE $ mkName "undefined")
         . VarT $ mkName n

  mk_typeable tv = ClassP (mkName $ "Typeable" ++ tn) [VarT $ tvName tv]
   where
    tn | tvArity tv == 1 = ""
       | otherwise       = show $ tvArity tv 

  mk_inst (InstanceD ctx typ decs)
    = TInstance (inp_cxt ++ ctx) typ $ map mk_dec decs
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
    proxy   = available "proxy"
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

  available prefix
    = fromJust
    . find (`notElem` allNames rep)
    $ map (mkName . (prefix++) . empty0) ([0..] :: [Int])

  empty0 0 = ""
  empty0 n = show n

buildTemplate _
  = error "Instance Template syntax must resemble class declarations."

-- Utils for buildTemplate

-- Note: Due to lack of TH support for constraint kinds, equality
-- constraints aren't supported.
predToType :: Pred -> Maybe Type
predToType (ClassP n ts) = Just $ foldl AppT (VarT n) ts
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

allNames :: Data d => d -> [Name]
allNames = listify (const True :: Name -> Bool)

-- The parser for deriving' quasi-quotes.

-- TODO: put this in different package, so that we can have people use it
--  without the Exts dependencies.

parseTemplate :: String -> TemplateRep
parseTemplate input
  = TemplateRep (head . fromLeft $ Exts.parseDecs top)
               (       fromLeft $ Exts.parseDecs insts)
 where
  (top, insts) = helper input

  helper :: String -> (String, String)
  -- Parse derived instance.
  helper (splitFind "instance" -> Just (leading,
            (splitFind "where" -> Just (typ, rest))))
    | leadingIndent > 0 = recurse (leading ++ "\n", decl ++ body ++ "\n") rest'
    | otherwise         = recurse (leading ++ decl, "")                   rest
   where
    leadingIndent = indentLevel $ reverse leading

    recurse p = bizip (++) (++) p . helper

    decl = "instance" ++ typ ++ "where"

    (body, rest') = case splitFind "{" rest of
      Just (pre, post) | all isSpace pre
        -> first (('{':) . (++"}"))
         . fromJust
         $ splitFind "}" post

      _ -> (unlines *** unlines)
         . break ((<= leadingIndent) . indentLevel)
         . tail
         $ lines (' ':rest)
  helper str = (str, "")

-- Utils for the templateParser

-- | Parse mode with all extensions and no fixities.
parseMode :: Exts.ParseMode
parseMode = Exts.ParseMode
  { Exts.parseFilename = ""
  , Exts.extensions = Exts.glasgowExts
                   ++ [Exts.TupleSections, Exts.BangPatterns, Exts.ViewPatterns]
  , Exts.ignoreLinePragmas = False
  , Exts.ignoreLanguagePragmas = False
  , Exts.fixities = Nothing
  }

fromLeft :: Either String a -> a
fromLeft = either error id

splitFind :: String -> String -> Maybe (String, String)
splitFind xs ys = second (drop $ length xs)
              <$> (find (isPrefixOf xs . snd) $ zip (inits ys) (tails ys))

-- NOTE: assumes that the string is already known to be whitespace.
indentLevel :: String -> Int
indentLevel = length . concatMap subst . takeWhile (`elem` " \t")
 where
  subst '\t' = replicate 8 ' '
  subst x = [x]

-- ekmett's bizip specialized to tuples
bizip :: (a1 -> a2 -> a3) -> (b1 -> b2 -> b3)
      -> (a1, b1) -> (a2, b2) -> (a3, b3)
bizip f g (x, y) (x', y') = (f x x', g y y')
