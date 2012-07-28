{-# LANGUAGE
    TemplateHaskell
  , RankNTypes
  , ConstraintKinds
  , FlexibleInstances
  #-}

-- | This example illustrates the simplest variety of implementing one
--   interface in terms of another - renaming a method.
module Test where

import Classes
import Language.Haskell.InstanceTemplates

$(instantiate
 [template Foo_T [t| Foo Int |]
   [d|
     getFoo = 5
   |]
 ])

main = print (getBar :: Int)
