{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ConstraintKinds, KindSignatures, TupleSections #-}
module Language.Haskell.InstanceTemplates 
  ( derive, deriving', Instance(..)
  , Typeable(..)
  ) where

import Control.Arrow        ( first, second, (***) )
import Control.Applicative  ( (<$>) )
import Control.Monad        ( unless )

import Data.Char     ( isSpace )
import Data.Function ( on )
import Data.List     ( groupBy, sortBy, find, isPrefixOf, inits, tails )
import Data.Maybe    ( fromJust, catMaybes )
import Data.Ord      ( comparing )
import Data.Typeable ( Typeable(..) )
import Debug.Trace   ( trace )

import GHC.Exts (Constraint)

-- import GHC.Exts (Constraint)
import Language.Haskell.TH
import Language.Haskell.TH.Quote       (QuasiQuoter(..))
import Language.Haskell.TH.Lift        (lift)
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Meta as Exts

deriving' = dquasi (mkDeriver . parseDeriver)

class Deriver a where
  derive :: a -> [Dec] -> [Dec]

data Instance (ctxt :: Constraint) = Instance


data DeriverRep = DeriverRep Dec [Dec]

instance Show DeriverRep where
  show (DeriverRep inp _) = pprint inp

mkDeriver :: DeriverRep -> DecsQ
mkDeriver (DeriverRep (ClassD inpCtx inpName inpTvs inpFds inpDecs) insts)
  | not (null inpFds) = error "Functional dependencies not allowed in instance templates."
--TODO: It /might/ be nice to be able to have instance derivers that generate no instances...
  | null insts = error "Instance deriver must generate some instances."
  | otherwise = do
      dd <- deriverDecl
      return [typeDecl, dd]
 where
  typeDecl = TySynD inpName inpTvs . mkTupleT
           $ catMaybes (map predToType inpCtx) ++ [t | (InstanceD _ t _) <- insts]

  -- Note: Due to lack of TH support for constraint kinds, equality constraints aren't
  -- supported.
  predToType (ClassP n ts) = Just $ foldl AppT (VarT n) ts
  predToType _ = Nothing

  headType = foldl AppT (ConT inpName) $ map (VarT . tvName) inpTvs
  
  -- TODO: resolve kind arities by reifying the constraints and doing kind
  -- inference.  For now * is assumed unless an explicit sig is given.
  deriverDecl :: DecQ
  deriverDecl = do
    dType      <- [t| Deriver (Instance $(return headType)) |]
    expr <- listE $ map (lift . mkInst) insts
    return
      . InstanceD (map mkTypeable inpTvs) dType
      . (:[])
      . FunD (mkName "derive")
      $ [Clause [WildP, VarP (mkName "x")] (NormalB expr) []]

  mkTypeable tv = ClassP (mkName $ "Typeable" ++ tn)
                         [VarT $ tvName tv]
   where
    tn | tvArity tv == 1 = ""
       | otherwise       = show $ tvArity tv 

  mkInst (InstanceD ctx typ decs)
    = InstanceD (inpCtx ++ ctx) typ decs

-- A few utils

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

parseType :: String -> Type
parseType = Exts.toType
          . fromLeft
          . Exts.parseResultToEither
          . Exts.parseTypeWithMode parseMode

parseDecs :: String -> [Dec]
parseDecs = (\(Exts.Module _ _ _ _ _ _ x) -> Exts.toDecs x)
          . fromLeft
          . Exts.parseResultToEither
          . Exts.parseModuleWithMode parseMode
          . ("module Dummy where\n" ++)

-- | Parse mode with all extensions and no fixities.
parseMode :: Exts.ParseMode
parseMode = Exts.ParseMode
  { Exts.parseFilename = ""
  , Exts.extensions = Exts.glasgowExts ++ [Exts.TupleSections, Exts.BangPatterns, Exts.ViewPatterns]
  , Exts.ignoreLinePragmas = False
  , Exts.ignoreLanguagePragmas = False
  , Exts.fixities = Nothing
  }

fromLeft :: Either String a -> a
fromLeft = either error id

dquasi :: (String -> DecsQ) -> QuasiQuoter
dquasi f = QuasiQuoter undefined undefined undefined f

splitFind :: String -> String -> Maybe (String, String)
splitFind on xs =   second (drop $ length on)
                <$> (find (isPrefixOf on . snd) $ zip (inits xs) (tails xs))

-- NOTE: assumes that the string is already known to be whitespace.
indentLevel :: String -> Int
indentLevel = length . concatMap subst . takeWhile (`elem` " \t")
 where
  subst '\t' = replicate 8 ' '
  subst x = [x]

-- ekmett's bizip specialized to tuples
bizip :: (a1 -> a2 -> a3) -> (b1 -> b2 -> b3) -> (a1, b1) -> (a2, b2) -> (a3, b3)
bizip f g (x, y) (x', y') = (f x x', g y y')

parseDeriver :: String -> DeriverRep
parseDeriver input
  = DeriverRep (head . fromLeft $ Exts.parseDecs top)
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

{-
mkDeriver :: DeriverRep -> DecsQ
mkDeriver (DeriverRep (InstanceD inpCtx inpTyp []) inpDecs insts) = do
  constraints <- tyConstraints inpTyp
  let vs = sndsEqual $ concatMap (uncurry tyVarArities) constraints

  -- Create a Deriver instance, where the provided variables must be typeable.
  method <- [d|derive _ x = $( listE $ map (lift . mkInst) insts ) |]
  let result = InstanceD (map mkTypeable vs)
        [t'| Language.Quasi.Instance.Deriver (Language.Quasi.Instance.Instance ({{ inpTyp }})) |]
        method
  
  return $ trace (pprint $ dequalifyNames result) [result]
 where
  mkTypeable (n, i)
    | i <  0 = error "Kind mismatch"
    | i == 0    = ClassP (mkName $ "Data.Typeable.Typeable"          ) [ConT n]
    | otherwise = ClassP (mkName $ "Data.Typeable.Typeable" ++ show i) [ConT n]

  mkInst (InstanceD ctx typ decs)
    = InstanceD (inpCtx ++ ctx) typ
    $ map withParams decs

  withParams
    = (\(FunD n [Clause p b _]) -> FunD n [Clause p b inpDecs])
    . dsFun



splitFind :: String -> String -> Maybe (String, String)
splitFind on xs =   second (drop $ length on)
                <$> (find (isPrefixOf on . snd) $ zip (inits xs) (tails xs))

deriverParser :: String -> Either String DeriverRep
deriverParser input = do
  let (splitFind "where" -> Just (before, after)) = inp
  header  <- parseDecs . debug $ "instance " ++ before ++ "where"
  inpDecs <- parseDecs . debug . unlines . map (dropWhile isSpace) . lines $ after
  instDecs <- parseDecs $ debug insts

  unless (length header == 1) . fail
    $ "Parse error in deriver. Need exactly one inp instance (got the following)\n"
    ++ pprint header

  return $ DeriverRep (head header) inpDecs instDecs
 where
  -- Split the input into code for the head instance and dependents.
  (inp, insts) = helper input

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


tyUnApp :: Type -> [Type]
tyUnApp = reverse . helper
 where
  helper (AppT l r) = r : tyUnApp l
  helper x          = [x]

tyFlatten :: Type -> [Type]
tyFlatten t = case tyUnApp t of
 (TupleT _ : xs) -> concatMap tyFlatten xs
 _               -> [t]

tyName :: Type -> Name
tyName (ConT n) = n

--TODO: handle partiality more precisely
tyConstraints :: Type -> Q [(Type, Info)]
tyConstraints = mapM (\t -> (t,) <$> reify (tyName . head $ tyUnApp t)) . tyFlatten

getInfoDec :: Info -> Dec
getInfoDec (ClassI  d _) = d
getInfoDec (TyConI  d  ) = d
getInfoDec (FamilyI d _) = d

getTyVars :: Dec -> [TyVarBndr]
getTyVars (DataD    _ _ xs _ _) = xs
getTyVars (NewtypeD _ _ xs _ _) = xs
getTyVars (TySynD     _ xs _  ) = xs
getTyVars (ClassD   _ _ xs _ _) = xs
getTyVars (FamilyD  _ _ xs _  ) = xs

-- PrimTyConI ?

tyVarArities :: Type -> Info -> [(Name, Int)]
tyVarArities ty i = zipWith getArity (tyFlatten ty) (getTyVars $ getInfoDec i)
 where
  getArity t (PlainTV  n  ) = (n, helper t 0)
  getArity t (KindedTV n k) = (n, helper t $ kindSize k)
  helper t n = n - length (tyUnApp t)

errIf :: (a -> Bool) -> a -> a
errIf f x | f x = undefined
          | otherwise = x

sndsEqual :: (Ord a, Eq b) => [(a, b)] -> [(a, b)]
sndsEqual = map head
          . errIf nonEqual
          . groupBy ((==) `on` fst)
          . sortBy (comparing fst)
 where
  nonEqual = not . all (\xs -> all id . zipWith ((==) `on` snd) xs $ tail xs)


deriver :: QuasiQuoter
deriver = dquasi (mkDeriver . fromLeft . deriverParser)

-}




{- DOH! lol.. Typeable.

class TypeRep1 a where
  typeRep1 :: a -> Type

class TypeRep2 a where
  typeRep2 :: a b -> Type

mkTypeRep :: Name -> DecsQ
mkTypeRep n = do
  info <- reify n
  return case info of
    TyConI (DataD    _ n' ts _ _) -> helper n' ts
    TyConI (NewtypeD _ n' ts _ _) -> helper n' ts
    -- TODO: Can we declare instances for these?
    PrimTyConI (n' _ _)           -> helper n' []
    -- TODO: but we can, right?
    _ -> error "Error: Cannot make TypeRep for non-datatype"
 where
  helper n' ts = InstanceD (map constraint ts) (typ $ reverse ts)
               $ [ FunD (mkName $ "typeRep" ++ arity) [NormalB expr]]
   where
    expr = 
  typ []     = ConT n'
  typ (x:xs) = AppT (typ xs) x
  constraint tv = ClassP (mkName $ "TypeRep" ++ arity) [ConT $ nameOf tv]
   where
    arity | PlainTV  _   <- tv = "1"
          | KindedTV _ k <- tv = show $ kindSize k

class Deriver a where
  derive :: a -> [Dec] -> [Dec]

-}
