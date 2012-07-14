{-# LANGUAGE TemplateHaskell, QuasiQuotes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}
module Classes where

import Language.Haskell.InstanceTemplates

import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

class Bar a where
  bar :: a

type Foo a = Bar a

$(mkTemplate =<<
  [d|class Foo a where { foo :: a }
     instance Bar a where { bar = foo } |]
 )