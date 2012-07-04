module Language.Haskell.InstanceTemplates.Common where

import Control.Monad ( liftM, msum )
import Data.Function ( on )
import Data.List     ( find, inits, isPrefixOf, tails, groupBy )
import Data.Maybe    ( fromJust )
import Debug.Trace   ( trace )

debug x = trace (show x) x

-- I'd use Data.List.Split, but it really doesn't do very well for this type
-- of splitting.

-- Splits when the integer is >= 0. Integer is the amount used for delim.
splitLen :: (String -> Int) -> String -> Maybe (String, String, String)
splitLen f xs
  = liftM helper
  . find ((>= 0) . fst)
  . map (\x -> (f $ snd x, x))
  $ zip (inits xs) (tails xs)
 where
  helper (cnt, (l, r)) = (l, pre, post)
   where
    (pre, post) = splitAt cnt r
 
splitFind :: String -> String -> Maybe (String, String)
splitFind on
  = liftM (\(l, _, r) -> (l, r))
  . splitLen (\xs -> if on `isPrefixOf` xs then length xs else -1)

splitFinds :: [String] -> String -> Maybe (String, String, String)
splitFinds ons = splitLen helper
 where
  helper rest = fromJust . msum . (++ [Just (-1)]) $ map match ons
   where
    match on | on `isPrefixOf` rest = Just $ length on
             | otherwise            = Nothing

--TODO: handle {- and -} in strings and quasi-quotes

removeComments :: String -> [String]
removeComments = map removeLineComment . lines . concat . removeBlockComments 0
 where
  removeLineComment line = maybe line fst $ splitFind "--" line
  removeBlockComments :: Int -> String -> [String]
  removeBlockComments n txt = helper $ splitFinds ["{-", "-}"] txt
   where
    helper :: Maybe (String, String, String) -> [String]
    helper Nothing                 = if0 txt
    helper (Just (pre, mid, post)) = if0 pre ++ removeBlockComments n' post
     where
      n' | mid == "{-" = n + 1
         | otherwise   = n - 1

      if0 x | n == 0    = [x]
            | otherwise = []

-- Take bracketed things and make them one-liners.
{-
processBrackets :: String -> String
processBrackets = recurse 0
 where
  recurse n txt = helper $ splitFinds ["{", "}"] txt
   where
   	helper Nothing            = txt
   	helper (Just (l, "{", r)) = 
   	helper (Just (l, "}", r)) = 
-}

data Indented = Indented Int String [Indented]
  deriving (Eq, Read, Show)

parseIndents :: [String] -> [Indented]
parseIndents [] = [Indented 0 "" []]
parseIndents ls = recurse 0 $ map (\l -> (level l, dropWhile isSpace l)) ls
 where
  recurse :: Int -> [(Int, String)] -> [Indented]
  recurse sub
    = map (\((ix, s):xs) -> Indented (ix - sub) s $ recurse (ix + sub) xs)
    . groupBy ((<) `on` fst)
  isSpace = (`elem` " \t")
  level :: String -> Int
  level = length . concatMap subst . takeWhile isSpace
   where
    subst '\t' = replicate 8 ' '
    subst x = [x]

-- `concatMap reIndent`  is a (forgetful) inverse for parseIndents

reIndent :: Indented -> [String]
reIndent = helper 0
 where
  helper acc (Indented ix str rest)
    = (replicate acc' ' ' ++ str) : concatMap (helper acc') rest
   where
   	acc' = acc + ix
