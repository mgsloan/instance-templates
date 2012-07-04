module Test1 where

class Foo a where
  foo :: a

class Bar a where
  bar :: a

instance Foo Int where
  foo = 0

instance Bar Double where
  bar = 0.0
