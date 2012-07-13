{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
module TestUse where

import Data.Generics.Text
import Language.Haskell.InstanceTemplates
import Language.Haskell.TH
import Test

$(instantiate
 [instance' [t| () |] (classHead :: Foo_Template (Int, TV_a) TV_b)
   [d|
     foo = (5, undefined)
   |]
 ])