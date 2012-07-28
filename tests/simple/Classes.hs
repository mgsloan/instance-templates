{-# LANGUAGE
    TemplateHaskell
  , ConstraintKinds
  , ScopedTypeVariables
  , FlexibleInstances
  #-}

-- | This example illustrates the simplest variety of implementing one
--   interface in terms of another - renaming a method.
module Classes where

import Language.Haskell.InstanceTemplates

import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

class Bar a where
  getBar :: a

type Foo a = Bar a

$(mkTemplate =<< [d|
  class Foo a where
    getFoo :: a
  instance Bar a where
    getBar = getFoo
 |])