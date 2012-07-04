{-# LANGUAGE TemplateHaskell, QuasiQuotes, ConstraintKinds, FlexibleInstances #-}
module Test where

import Language.Haskell.InstanceTemplates

class Bar a where
  bar :: a

[deriving'|class Foo a where
  foo :: a
  instance Bar a where
  	bar = foo
|]