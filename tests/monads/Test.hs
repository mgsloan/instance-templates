{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RankNTypes, ConstraintKinds #-}
module Test where

import qualified Classes as C
import Templates
import Language.Haskell.InstanceTemplates
import Prelude (zipWith, repeat, ($), const, id)

newtype ZipList a = ZipList [a]

$(instantiate
 [instance' Applicative_Template [t| Applicative ZipList |]
   [d|
    (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)
    pure x                        = ZipList (repeat x)

    (*>) :: C.Applicative f => f a -> f b -> f b
    (*>) = C.liftA2 (const id)
    (<*) :: C.Applicative f => f a -> f b -> f a
    (<*) = C.liftA2 const
   |]
 ])