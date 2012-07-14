{-# LANGUAGE TemplateHaskell, QuasiQuotes, RankNTypes, ConstraintKinds, FlexibleInstances #-}
module Test where

import Classes (Foo, Foo_Template(..))
import Language.Haskell.InstanceTemplates

$(instantiate
 [template Foo_Template [t| forall b. Foo Int |]
   [d|
     foo = 5
   |]
 ])