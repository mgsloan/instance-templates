{-# LANGUAGE InstanceTemplates #-}

-- | This example illustrates the simplest variety of implementing one
--   interface in terms of another - renaming a method.

--   This file is a sketch of what this code would look like with
--   -XInstanceTemplates.
module WithExt where


class Bar a where
  getBar :: a

deriving class Foo a where 
  getFoo :: a
  instance Bar a where
    getBar = getFoo


-- Usage

instance Foo Int where
  getFoo = 5

main = print (getBar :: Int)


-- The above code desugars to:
type Foo a = Bar a

instance Bar Int where
  getBar = getFoo
   where
    getFoo = 5