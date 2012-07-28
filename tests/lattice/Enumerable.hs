{-# LANGUAGE TemplateHaskell, ConstraintKinds, ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}

-- From Max Bolingbroke's lattices library
module Enumerable where

import Language.Haskell.InstanceTemplates
import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

-- | Finitely enumerable things
class Enumerable a where
    universe :: [a]

type BoundedUniverse a = (Enum a, Bounded a, Enumerable a)

$(mkTemplate =<< [d|
  class (Enum a, Bounded a) => BoundedUniverse a where

  instance Enumerable a where
    universe = enumFromTo minBound maxBound

 |] )

-- TODO: add to this rather sorry little set of instances. Can we exploit commonality with lazy-smallcheck?


instance Enumerable a => Enumerable (Maybe a) where
    universe = Nothing : map Just universe

instance (Enumerable a, Enumerable b) => Enumerable (Either a b) where
    universe = map Left universe ++ map Right universe

instance Enumerable () where
    universe = [()]

instance (Enumerable a, Enumerable b) => Enumerable (a, b) where
    universe = [(a, b) | a <- universe, b <- universe]
