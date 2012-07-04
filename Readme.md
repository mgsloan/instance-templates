Instance Templates
==================

This goes a little bit beyond "re-skinning" Haskell, however is fairly
equivalent to introducing TH functions of the variety that are already pretty
popular: deriving class instances.

The purpose of these declarations is to provide a mechanism for expressing how
a set of interfaces can be expressed in terms of some other interface.  This
allows us to restructure class hierarchies without breaking client code, and
reduce code duplication found in boilerplate instances.

```haskell
deriving class PreOrder a where
  (<=) :: a -> a -> Bool

  instance Eq a where
    x == y = (x <= y) && (y <= x)

  instance Ord a where
    (<=) = (<=)
```

The immediately nested `(<=)` signature declares a "parameter" of the
"instance template".  These are called parameters because they are only used to specialize the generated instances, and don't get exported.

The `(<=)` nested inside `instance Ord` is not ambiguous with the outer one,
because the template parameters shadow the methods defined within (method
definitions can reference each other, though).

In order to use the "deriving class PreOrder" declaration, we write code that
looks like this:

```haskell
instance PreOrder Bool where
  False <= _ = True
  True  <= x = x
```

The methods defined in the instance are used as the parameters to the instance
template, expanding into:

```haskell
instance Eq a where
  x == y = (x <= y) && (y <= x)
   where
    False <= _ = True
    True  <= x = x

instance Ord a where
  (<=) = (<=)
   where
    False <= _ = True
    True  <= x = x
```

This is the trivial, definitional desugaring - the compiler could certainly do something more clever.

It'd be nice to still have the property that instance heads can be used as
constraints in polymorphic types.  As such, I think it's reasonable for the
above definition to implicitly create the following constraint-kind synonym:

```haskell
type PreOrder a = (Eq a, Ord a)
```

Resolving Overlap
-----------------

We can now make instances mean something different, and generate more class instances than before.  This has a few implications:

* The generated instances might conflict with those already defined in the
  module.  In this case, I think that this is a very reasonable mechanism for
  "opt-ing out" of a particular generated instance.  However, it could be
  unexpected, so a pragma-suppressible warning should be generated for this
  sort of overlap.

* The instances a particular module exports is now dependent on the way the
  classes are defined in its imports. This is not as much of an issue as it
  sounds - an instance of `Monad` will still mean we have an instance of
  `Monad` - be it a normal or compound class constraint.

  However, this is an issue when it comes to orphan instances, as identical
  library / client code could have an instance clash, given a different set of
  definitions for the library's dependencies.I think that this is acceptable,
  as orphan instances are known to be dangerous, and many instance templates
  would not have this sort of behavior.

* The generated instances might conflict with those produced by other template
  invocations.  This is something that we'd quite reasonably want to do, while
  re-working entire hierarchies.  The rest of this section is devoted to this
  issue.

For example, we could write an instance template for the old version of
`Applicative`, that generated the new, properly hierarchical versions of
`Functor` and `Applicative`.  We could then also write an instance template
for `Monad` that generated all three.

The problem with this is that code using the current hierarchy would quite
often reasonably have instances for all three, so two versions of
`Applicative` would be generated. So, the question becomes how we can tell
which definition supersedes, without hackily defining an arbitrary priority
order.

A very reasonable answer is suggested by analogy to the solution used to deal
with conflicts with regular instances - have explicit invocations of the
instance template supersede those specified inside an instance template.
For example:

```haskell
deriving class Foo a where
  foo :: a

deriving class Bar a where
  bar :: a -> a
  baz :: a

  instance Foo a where
    foo = bar baz

  instance Enumerable a where
    enumerate = baz : map bar enumerate
```

If we have:

```haskell
instance Bar Int where
  bar = (+1)
  baz = 0
```

Then the following gets generated (for clarity, the definitions that would be in
where declarations have been substituted):

```haskell
instance Foo Int where
  foo = (+1) 0

instance Enumerable Int where
  enumerate = 0 : map (+1) enumerate
```

If, instead we use both instance templates:

```haskell
instance Foo Int where
  foo = 2

instance Bar Int where
  bar = (+1)
  baz = 0
```

Then we get:

```haskell
instance Foo Int where
  foo = 2

instance Enumerable Int where
  enumerate = 0 : map (+1) enumerate
```

So, we've manged to be able to mix and match instance templates!


Why?
====

* It's simple.  We're just supplying values to a generic instance, to create a
  specific one, and these parameters are referentially transparent.

* More powerful instance derivation allows us to mitigate the impact of
  historical decisions.

  Being able to rework, say, the Numeric class hierarchy, is the main goal of
  this proposal (and those that came before):
  http://hackage.haskell.org/trac/ghc/wiki/DefaultSuperclassInstances
    
  As mentioned in that page, default superclass instances have been a "matter
  of consternation" for some time, as no approach to the problem has been
  satisfying enough to be implemented.  By forcing the decision of how to
  implement a class to be per-datatype, we avoid attempting to define
  typeclass instances which "always" can be implemented universally in terms
  of some other.

* In order to create configurable instances (see the "weak typing" section)
  with superclass instances, we need to create a new typeclass.  This is odd,
  because it doesn't make much sense to use it in any other context than
  the superclass of the instance.  I think it's preferable to be able to "hide"
  this class so that it can't be depended on.

* Avoidance of TH.

  - People have observed many things that are wrong with TH / mis-aligned with
    Haskell philosophy.
    http://stackoverflow.com/questions/10857030/whats-so-bad-about-template-haskell/

  - This feature can be implemented as a TH library, though requiring special
    delineation around usages.  However, the error messages and potential for
    analysis by tools would be impaired.  By making it a language feature, we
    can conquer even more of the useful regions of macro-expansion design space
    and reduce the necessity of TH.

  - Compared to the power and complexity of TH, this feature is very simple,
    and we can rely on the reasoning power of referential transparency.


Examples
========

Splitting out Eq / Show
-----------------------

With GHC 7.4, one of the first breaking changes (in a while, anyway) was made
to the Prelude - removing the Eq and Show superclass constraints.  Here's how
this feature would have made the change non-breaking:

```haskell
-- In Prelude

import qualified NewPrelude as N

deriving class (Show a, Eq a) => Num a where
  instance N.Num a

deriving class (Num a, Ord a) => Real a where
  instance N.Real a

deriving class (Real a, Enum a) => Integral a where
  instance N.Integral a

deriving class (Num a) => Fractional a where
  instance N.Fractional a

deriving class (Real a, Fractional a) => RealFrac a where
  instance N.RealFrac a
```

Here, the instances which contain no "where" clause indicate that the methods
of the instance are implicitly made into parameters of the template.  I'm not
sure if this is a good syntactic choice, but a mechanism for doing this is
_very_ handy for the situations in which you wish to modify a class hierarchy
without breaking clients.

Eq / Show is a fairly minimal change to the numeric hierarchy, but it required
a lot of declarations because it happened at the root of the hierarchy.

Redundant Enum instances for RealFloat
--------------------------------------

```haskell
instance  Enum Float  where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTos

instance  Enum Double  where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTo
```

A perfect application of instance templates!  These two instance declarations
are identical. In general, we can use this as a template for a potential
way of getting an implementation of `Enum` for any `RealFrac` implementor.
Here's the implementation using templates:

```haskell
deriving class RealFrac a => RealFracEnum a where
  instance Enum a where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTo

instance RealFracEnum Float where

instance RealFracEnum Double where
```

Fine Grained Numerics
---------------------

A more fine-grained splitting of the numeric hierarchy might look something
like this:

```haskell
class Addable       a where (+)    :: a -> a -> a
class Multiplicable a where (*)    :: a -> a -> a
class Subtractable  a where (-)    :: a -> a -> a
class Negateable    a where negate :: a -> a
class Absable       a where abs    :: a -> a
class Signumable    a where signum :: a -> a

class FromIntegerable a where
  fromInteger :: a -> a

deriving class Num a where
  instance Addable         a
  instance Multiplicable   a
  instance Subtractable    a
  instance Negateable      a
  instance Absable         a
  instance Signumable      a
  instance FromIntegerable a
```

This can be made even more general:

```haskell
-- ...

class Subtract a b where
  type SubtractResult a b :: *
  (-) :: a -> b -> SubtractResult a b

class Negate a where
  type NegateResult a :: *
  negate :: a -> NegateResult a

-- ...

deriving class Subtractable a where
  (-) :: a -> a -> a
  instance Subtract a a where
    type SubtractResult a b = a
    (-) = (-)

deriving class Negateable a where
  negate :: a -> a
  instance Negate a where
    type NegateResult a = a
    negate = negate

-- ...

deriving class Num a where
  instance Addable         a
  instance Multiplicable   a
  instance Subtractable    a
  instance Negateable      a
  instance Absable         a
  instance Signumable      a
  instance FromIntegerable a
```

Note that the `Num` template remained unchanged, despite all of the methods
now having the most general type that's still (somewhat) useful!  Also, any
instances  written using the prior structure would also still work.
Unfortunately, this particular example might break some polymorphic code.
However, the effect shouldn't be that extensive.

Functor - Applicative - Monadic
-------------------------------
```haskell
module NewPrelude where

-- Similar to http://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

class Functor f where
  map :: (a -> b) -> f a -> f b
 
class Functor f => Applicative f where
  return :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a

  a *> b = map (const id) a <*> b
  a <* b = map const a <*> b
 
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  x >>= f = join $ map f x

  join :: m (m a) -> m a
  join x = x >>= id
 
class Monad m => MonadFail m where
  fail :: String -> m a
```

```Haskell
module Prelude where

import qualified NewPrelude as N

-- ...

deriving class Functor f where
  fmap :: a -> f a
  instance N.Functor f where
    map = fmap

deriving class Applicative f where
  pure   :: a -> f a
  (<*>)  :: f (a -> b) -> f a -> f b
  (*>)   :: f a -> f b -> f b
  (<*)   :: f a -> f b -> f a

  a *> b = const id <$> a <*> b
  a *> b = const    <$> a <*> b

  -- Note! This instantiates the template above
  instance Functor f where
    fmap f x = pure f <*> x

  instance N.Applicative f where
    return = pure
    (<*>)  = (<*>)
    (*>)   = (*>)
    (<*)   = (<*)

deriving class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a ->       m b  -> m b
  return :: a      -> m a
  fail   :: String -> m a

  m >> k = m >>= \_ -> k
  fail = error

  -- Note! This instantiates the template above
  instance Applicative m where
    return = return
    l <*> r = l >>= \f ->
              r >>= \x -> return (f x)

  instance N.Monadic m where
    (>>=) = (>>=)

  instance N.MonadFail m where
    fail = fail
```


"Problems"
==========

No potential solution to the "default superclass instances" is without its
trade-offs.  I think that the described solution is a straightforward,
understandable solution to the problem, that buys a lot of power, with
comparatively minimal issues:

* We cannot create instances that are automagically derived in the presence of
  some set of constraints.  This was the intent of the problematic "superclass-
  defaults" proposals, and is a huge can of worms.  I think that wanting this
  feature is a little over-reaching, if you want to maintain Haskell's current
  typeclass semantics.

* Cannot "combine" multiple instances, using their methods as parameters to
  the instance template.  (See "API Deltas" section below)

* Ideally we'd be able to seamlessly use old code with our new typeclasses, in
  concordance with "Design goal 1" mentioned in the "Other Proposals" section.
  However, this means that this feature would need to only have a LANGUAGE
  extension option (e.g. -XInstanceTemplates) for the modules defining
  instance templates.

  Are we comfortable changing the meaning of code without additional pragma,
  in the event that the dependencies specify this pragma?  Is there any
  precedent for language extensions doing this?

  If ConstraintKinds is used as the mechanism that allows for naming the set
  of derived instances, then this would also mean that -XConstraintKinds would
  need to be implicit in users of instance templates.  This is not so
  disastrous, though - as this could be restricted to usage of constraint
  kinds, and not actual declaration of them / usage of "Constraint".



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

Due to Haskell's power of abstraction for functions, we can straightforwardly
represent any function related API change that wouldn't break clients. Here,
module B is the "newer" module, and module A exports the old interface (I'm
omitting the import of B into A, and everything else that's unnecessary):

```haskell
-- Add a function
module A (foo) where -- ...

module B where foo = -- ...
               bar = -- ...


-- Rename a function
module A where bar = B.foo

module B where foo = -- ...


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
module A (foo, bar) where -- ...

module B1 where foo
module B2 where bar
```

Type Deltas
-----------

As we'd hope to find, there are similar patterns for types:

```haskell
-- Add a type
module A (Foo) where -- ...

module B where type Foo = -- ...
               type Bar = -- ...


-- Rename a type
module A where type Bar = Foo

module B where type Foo = -- ...


-- Permute parameters
module A where type Foo a b = B.Foo b a

module B where type Foo a b = -- ...


-- Add a parameter for more generality
module A where type Foo a b = B.Foo a b Bar

module B where type Foo a b c
               data Bar


-- Split types into different modules
module A (Foo, Bar) where -- ...

module B1 where type Foo
module B2 where type Bar
```

ADT constructors are quite a bit bumpier - I go into more detail about this in
the above blog post.


Instance Deltas
---------------

Here's the same analysis as above, but for instances:

```haskell
-- Adding instance while perfectly preserving the old is not possible
-- (implicitly import)


-- Renaming / permuting / adding parameters all don't make sense here.


-- We can weaken a constraint, but the instance exported from A won't have the
-- more restrictive constraints (this seems fine - TODO could it break things?)


-- Split instances into different modules.
-- This we can do!  But this is the path of the orphan instance!
module A where import B1; import B2

module B1 where instance A Blah
module B2 where instance B Blah
```

Constraint Deltas
-----------------

How about type-constraints?  Well, until this last year, doing these sorts of
things wasn't possible.  With the addition of Constraint-Kinds, we now can do
many of them:

```haskell
-- Add a class
module A (Foo) where -- ...

module B where class Foo a where -- ...
               class Bar a where -- ...


-- Rename a class
{-# LANGUAGE ConstraintKinds #-}
module A where type Bar = B.Foo

module B where class Foo a where -- ...


-- Permute parameters
{-# LANGUAGE ConstraintKinds #-}
module A where type Foo a b = B.Foo b a

module B where class Foo a b where -- ...


-- Add a parameter for more generality
{-# LANGUAGE ConstraintKinds #-}
module A where type Foo a b = Foo a b Bar

module B where class Foo a b c
               data Bar


-- Weaken a superclass constraint
{-# LANGUAGE ConstraintKinds #-}
module A where type Foo a = (Foo a, Eq a)

module B where class Foo a


-- Split classes into different modules
module A (Foo, Bar) where -- ...

module B1 where type Foo
module B2 where type Bar
```

When introducing the above example, I was careful to say "type-constraints",
not "type-classes".  This is because these API changes are leaky - they only
help with constraints, but not with the actual classes used for instances.


Class Deltas
------------

Instance templates allow us to continue this pattern of being able to express
API deltas for different Haskell declarations, but this time without the
leakiness needed to be able to declare instances.

```haskell
-- Rename a class
{-# LANGUAGE InstanceTemplates #-}
module A where deriving class Bar a where instance B.Foo a

module B where class Foo a where -- ...


-- Permute parameters
{-# LANGUAGE InstanceTemplates #-}
module A where deriving class Foo a b where instance B.Foo b a

module B where class Foo a b where -- ...


-- Add a parameter for more generality
{-# LANGUAGE InstanceTemplates #-}
module A where deriving class Foo a b where instance B.Foo a b Bar

module B where class Foo a b c
               data Bar
```

Method Deltas
-------------

This is where we really get into new territory.

```haskell
-- Add a method - this was possible before!
module A (Foo(foo)) where

module B where
class Foo a where
  foo :: a
  baz :: a
  baz = foo

-- However, now it's better, because "baz" doesn't appear as a potential method
{-# LANGUAGE InstanceTemplates #-}
module A where
deriving class Foo a where
  foo :: a
  instance B.Foo a where foo = foo


-- Rename a method
{-# LANGUAGE InstanceTemplates #-}
module A where
bar = B.foo

deriving class Foo a where
  bar :: a
  instance B.Foo a where foo = bar

module B where class Foo a where foo :: a


-- Permute parameters
{-# LANGUAGE InstanceTemplates #-}
module A where
bar x y = B.foo y x

deriving class Foo a where
  bar :: a -> a -> a
  instance B.Foo a where foo y x = bar x y

module B where class Foo a where foo :: a -> a -> a


-- Add a previously constant parameter
{-# LANGUAGE InstanceTemplates #-}
module A where
bar x y = B.foo x y eps

deriving class Foo a where
  bar :: a -> a -> a
  instance B.Foo a where foo x y = bar x y eps

module B where class Foo a where foo :: a -> a -> Double -> a
               eps :: Double


-- Weaken a superclass constraint
{-# LANGUAGE InstanceTemplates #-}
module A where
foo :: Eq a => a
foo = B.foo

deriving class Foo a where
  foo :: Eq a => a
  instance B.Foo a where foo = foo

module B where class Foo a where foo :: a


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
```

Scope-Restricted Weak Typing
============================

This is not at all a crucial point of my proposal, however, it is the kind of
thinking about instances that falls out of having a capability like this.

It's very tempting to give your language the 'intelligence' to implicitly apply
a function in order to resolve what would otherwise be a type error.  Common
examples of weak typing found in other languages are `Int -> Float`,
`Bool -> Int`, `Int -> String`, `String -> Int` and even `Float -> Int`.

The main example of this is anything that's newtype-ish - where we have
`a -> b` and `b -> a`.  An instance template could take these two functions and
yield the instance that would have been generated if you could inject a custom
constructor / destructor into GeneralizedNewtypeDeriving.  For example, Num:

```haskell
deriving Num b => BijNum a b where
  f :: a -> b
  g :: b -> a

  instance Num a where
    x + y         = g (f x + f y)
    x * y         = g (f x * f y)
    x - y         = g (f x - f y)
    negate      x = g (negate      (f x))
    abs         x = g (abs         (f x))
    signum      x = g (signum      (f x))
    fromInteger x = g (fromInteger (f x))
```

We can automatically generate this definition by processing the type signatures
of the methods in the class.  Parameters that are `a` should have `f` applied
to them, and results of type `a` should have `g` applied to them.

By extending this rewriting to more cases, we can get more sophisticated
templates.  For example, it could be specified that a result of type `f a`
should have `fmap g` applied to it.  By giving a notation for specifying this
rewriting, we are conceptually introducing the convenience of weak-typing, but
properly scoped to a known set of definitions / value junctures.


Relationship to Other Proposals
===============================

(This section is incomplete)

The question might be asked "Why wasn't this thought of before?".  The answer
is "I'm not sure", but I think that it likely has been thought about before,
just disregarded due to some preconceived notions of a desirable mechanism.
By focusing on extending the typeclass system directly, we end up with a very
complicated set of trade-offs, that are tough to navigate.  Instead, this
approach adds an entirely new type of declaration, and adds a new meaning for
instance declarations.  Rather than "changing" the meaning of instances, we
are allowing them to be applied to something they couldn't before - constraint
types.


Here's a design goal from the superclass instances write-up. It's given
as the reason that an "Opt-In" scheme such as this is undesirable,
without much further explanation.
  
> Design goal 1: a class C can be re-factored into a class C with a
> superclass, without disturbing any clients.

I think that this is still quite possible with an Opt-In scheme, we
just need to make instance declarations potentially mean something
quite different than before (when applied to constraint synonyms).


http://www.haskell.org/haskellwiki/Superclass_defaults

This proposal, and mine, play quite nicely with constraint synonyms -
instance templates can have a compound class constraint in the type
argument.

Where this proposal falls flat is that it still relies on the
defaulting system as its mechanism, leading to strange things:

> If both Class1 and Class2 have a default implementation, and Class1
> is a (indirect) superclass of Class2, then the default from Class1
> is ignored.

Also, by trying to wedge superclass defaults into the existing syntax,
we end up with a ton of funky restrictions:

> Subject to the constraint that:
> * No class appears more than once in the list.
> * The arguments to each class are the same.
> * ... the superclass relation gives a connected acyclic graph with a
    single source, the most specific class in the hierarchy.

This is also a weakness in the Strathyclyde Haskell Enhancement's
implementation of default superclass instances.


Musings on Default Superclass Instances
=======================================

Even in the presence of instance templates, default superclass instances would
be a very useful feature.  With instance templates, we can say "Oops, we
messed up!", and modify the library that defines a particular class without
disturbing clients - an excellent step forward. However, if we want to be able
to have clients of that class be able to use our library (which uses a
different typeclass), with no further work or modification of that library,
then we're in trouble.

At once, we need to worry about the case in which the user wants to override
this instance with a more efficient implementation.  In order to support this,
we need to make these superclass instances a very different sort of instance,
that can be "overrided" (this is the "default" part of "superclass defaults").

If we still want to support the following features of Haskell typeclasses:

* Orphan instances

* Consistency regarding which instances are used for a particular set of
  monomorphic types

Then we need to have global knowledge of our program when deciding when to
override default superclass instances.  This is fine, because we can do it at
link time, however it violates the currently held expectation that we know
locally how an instance is implemented.  Such is the price of convenience!

These are really quite different beasts than standard instances, and so I
would suggest that they have distinct syntax.  My proposal for instance
templates suggests that the following might be reasonable:

```haskell
deriving class Monad a => where
  instance Functor a where
    fmap f = (>>= return . f)
```

In other words, default superclass instances are just anonymous instance
templates!

The next problem is that these is there's ambiguity in the case that these
anonymous instance templates conflict.  I have an intuition that the scheme
used to resolve this with the named instance templates might be brought to
bear on this problem.

Credits
=======

The following people (and more that go unmentioned!) all gave me invaluable
encouragement, feedback, and insights in the process of refining this document
and its ideas:

* Edward Kmett (edwardk)
* Luite Stegeman (luite)
* Drew Day (tgeeky)
* William Cauchois (wcauchois)
