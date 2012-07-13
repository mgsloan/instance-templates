{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}
module Test where
import Templates
import Language.Haskell.InstanceTemplates

newtype ZipList a = ZipList [a]

$(instantiate
 [instance' [t| () |] (classHead :: Functor_Template (ZipList TV_a))
   [d|
    (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)
    pure x                        = ZipList (repeat x)
   |]
 ])