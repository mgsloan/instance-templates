API Deltas
==========
http://www.mgsloan.com/wordpress/?p=219

In this somewhat-rough, yet extensive blog post, I describe a solution to
dependency hell.  It's not a new one - the idea is to export compatibility
modules.  This frees the library developer to develop the API without worrying
as much about client code.

This offers a different perspective of what it can mean to write Haskell
modules of a particular form - they can express "API Deltas".  In other words,
they can introduce a set of definitions, in terms of some other API, that
encode the rewriting necessary to target that other API.

This perspective of Haskell as API deltas is what led me to the idea of
instance templates.


Function Deltas
---------------

With Haskell's module system and function declarations, we can
straightforwardly represent any function related API change that wouldn't
break clients . Here, module B is the "newer" module, and module A exports the
old interface:

```haskell
-- Add a function
module A (foo) where import B

module B where foo = ...
               bar = ...


-- Rename a function
module A where bar = B.foo

module B where foo = ...


-- Permute parameters
module A where foo :: Int -> Double -> Double
               foo x y = B.foo y x

module B where foo :: Double -> Int -> Double


-- Add a previously constant parameter
module A where foo x y = B.foo x y eps

module B where foo :: Double -> Int -> Double -> Double
               eps :: Double


-- Weaken a constraint
module A where foo :: (Num a, Eq a, Show a) => a
               foo = B.foo

module B where foo :: (Num a) => a


-- Split functions into different modules
module A (foo, bar) where ...

module B1 where foo
module B2 where bar


-- Merge functions into the same module
module A1 (foo) where import B
module A2 (bar) where import B

module B (foo, bar) where ...
```

Type Deltas
-----------

As we'd hope to find, there are similar patterns for types:

```haskell
-- Add a type
module A (Foo) where import B

module B where type Foo = ...
               type Bar = ...


-- Rename a type
module A where type Bar = B.Foo

module B where type Foo = ...


-- Permute parameters
module A where type Foo a b = B.Foo b a

module B where type Foo a b = ...


-- Add a parameter for more generality
module A where type Foo a b = B.Foo a b Bar

module B where type Foo a b c
               data Bar


-- Split types into different modules
module A (Foo, Bar) where import B1; import B2

module B1 where type Foo
module B2 where type Bar


-- Merge types into the same module
module A1 (Foo) where import B
module A2 (Bar) where import B

module B where type Foo = ...
               type Bar = ...
```

ADT constructors are quite a bit bumpier - I go into more detail about this in
the above blog post.


Constraint Deltas
-----------------

How about type-constraints?  Well, until this last year, doing these sorts of
things wasn't possible.  With the addition of Constraint-Kinds, we now can do
many of them:

```haskell
-- Add a class
module A (Foo) where import B

module B where class Foo a where ...
               class Bar a where ...


-- Rename a class
{-# LANGUAGE ConstraintKinds #-}
module A where type Bar = B.Foo

module B where class Foo a where ...


-- Permute parameters
{-# LANGUAGE ConstraintKinds #-}
module A where type Foo a b = B.Foo b a

module B where class Foo a b where ...


-- Add a parameter for more generality
{-# LANGUAGE ConstraintKinds #-}
module A where type Foo a b = Foo a b Bar

module B where class Foo a b c
               data Bar


-- Weaken a superclass constraint
{-# LANGUAGE ConstraintKinds #-}
module A where type Foo a = (B.Foo a, Eq a)

module B where class Foo a


-- Split constraints into different modules
module A (Foo, Bar) where -- ...

module B1 where class Foo
module B2 where class Bar


-- Merge constraints into one modules
module A1 (Foo) where import B
module A2 (Bar) where import B

module B where class Foo
               class Bar
```

When introducing the above example, I was careful to say "type-constraints",
not "type-classes".  This is because these API changes are leaky - they only
help with constraints, but not with the actual classes used for instances.


Class Deltas
------------

Instance templates allow us to continue this pattern of being able to express
API deltas for different Haskell declarations, but this time without the
leakiness needed to be able to declare instances.  Here are the different
relevant re-factorings:

```haskell
-- Rename a class
{-# LANGUAGE InstanceTemplates #-}
module A where deriving class Bar a where instance B.Foo a

module B where class Foo a where ...


-- Permute parameters
{-# LANGUAGE InstanceTemplates #-}
module A where deriving class Foo a b where instance B.Foo b a

module B where class Foo a b where ...


-- Add a parameter for more generality
{-# LANGUAGE InstanceTemplates #-}
module A where deriving class Foo a b where instance B.Foo a b Bar

module B where class Foo a b c where ...
               data Bar = ...
```

Method Deltas
-------------

This is where we really get into the more powerful territory of this idea.

```haskell
-- Add a method - this was possible before!
module A (Foo(foo)) where

module B where
class Foo a where
  foo :: a
  baz :: a
  baz = foo

-- However, now it's better, because now "baz" doesn't appear as a potential 
-- method to declare in an instance.
{-# LANGUAGE InstanceTemplates #-}
module A where
deriving class Foo a where
  foo :: a
  instance B.Foo a where
    foo = foo


-- Rename a method
{-# LANGUAGE InstanceTemplates #-}
module A where
bar = B.foo

deriving class Foo a where
  bar :: a
  instance B.Foo a where
    foo = bar

module B where
class Foo a where 
  foo :: a


-- Permute parameters
{-# LANGUAGE InstanceTemplates #-}
module A where
bar x y = B.foo y x

deriving class Foo a where
  bar :: a -> a -> a
  instance B.Foo a where
    foo y x = bar x y

module B where
class Foo a where
  foo :: a -> a -> a


-- Add a previously constant parameter
{-# LANGUAGE InstanceTemplates #-}
module A where
bar x y = B.foo x y eps

deriving class Foo a where
  bar :: a -> a -> a
  instance B.Foo a where
    foo x y _ = bar x y

module B where
  class Foo a where
    foo :: a -> a -> Double -> a
    eps :: Double


-- Weaken a superclass constraint
{-# LANGUAGE InstanceTemplates #-}
module A where
foo :: Eq a => a
foo = B.foo

deriving class Eq a => Foo a where
  foo :: a
  instance B.Foo a where foo = foo

module B where
  class Foo a where
    foo :: a


-- Split methods into different classes
module A where
deriving class Foo a where
  instance B.Foo a
  instance B.Bar a

module B where
class Foo a where
  foo :: a
class Bar a where
  bar :: a


-- Merge methods into one "class" (really an instance template)
module A (Foo(foo), Bar(bar)) where import Internal

module B (Merge(foo, bar)) where import Internal

module Internal where
class Foo a where foo :: a
class Bar a where bar :: a

deriving class Merge a where
  instance Foo a
  instance Bar a
```


Instance Deltas
---------------
TODO: this section will likely be removed!

Here's the same analysis as above, but for instances:

```haskell
-- Adding an instance while preserving the old interface is not possible
-- (orphan instances)


-- Renaming / permuting / adding parameters all don't make sense here.


-- We can weaken a constraint, but the instance exported from A won't have the
-- more restrictive constraints (this seems fine - TODO could it break things?)


-- Split instances into different modules.
-- This we can do!  But this is the path of the orphan instance!
module A where import B1; import B2

module B1 where instance A Blah
module B2 where instance B Blah


-- Merge instances into the same module.
-- This we can't do (without making both modules in the new interface export 
-- the instances)
```