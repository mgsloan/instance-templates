{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module TestUse where

import Test
import Language.Haskell.InstanceTemplates

$(instantiate
 [instance' [t| () |] (classHead :: Foo_Template (Int, TV_a) TV_b)
   [d|
     foo = (5, undefined)
   |]
 ])