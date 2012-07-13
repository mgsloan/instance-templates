{-# LANGUAGE TemplateHaskell, QuasiQuotes, ConstraintKinds, ScopedTypeVariables #-}
module Test where

import Language.Haskell.InstanceTemplates
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class Bar a where
  bar :: a

[deriving'| class Foo a b where
  foo :: a
  instance Bar a where
    bar = foo
|]

-- Generated code:
{-
type Foo a b = Bar a
data Foo_Template a b
instance (Typeable a, Typeable b) => Template (Foo_Template a b) where
  invokeTemplate _ cxt decls
    = DeriverOutput (AppT (AppT (ConT $ mkName "Foo") ty_a) ty_b)
    $ [DInstance
         cxt
         (AppT
            (ConT (Name (mkOccName "Bar") NameS))
            ty_a)
         [ValD
            (VarP (Name (mkOccName "bar") NameS))
            (NormalB
               (LetE
                  [ValD
                     (VarP (Name (mkOccName "proxy") NameS))
                     (NormalB (VarE (Name (mkOccName "foo") NameS)))
                     []]
                  (VarE (Name (mkOccName "proxy") NameS))))
            decls]]
   where
    ty_a = tvType (undefined :: a)
    ty_b = tvType (undefined :: b)
-}