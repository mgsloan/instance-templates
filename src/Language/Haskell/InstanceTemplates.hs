{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns, ConstraintKinds, KindSignatures, TupleSections #-}
module Language.Haskell.InstanceTemplates 
  ( derive, deriving', Instance(..)
  , Typeable(..), parseType
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

-- The public API.

deriving' = QuasiQuoter undefined undefined undefined
                        (buildDeriver . parseDeriver)


class Deriver a where
  derive :: a -> [Dec] -> [Dec]

data Instance (ctxt :: Constraint) = Instance

parseType :: String -> Type
parseType = Exts.toType
          . fromLeft
          . Exts.parseResultToEither
          . Exts.parseTypeWithMode parseMode


-- The result of parsing.

data DeriverRep = DeriverRep Dec [Dec]

buildDeriver :: DeriverRep -> DecsQ
buildDeriver (DeriverRep (ClassD inpCtx inpName inpTvs inpFds inpDecs) insts)
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

-- Utils for buildDeriver

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


-- The parser for deriver' quasi-quotes.

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

-- Utils for the deriverParser

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
