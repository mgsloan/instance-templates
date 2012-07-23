Examples
========

Splitting out Eq / Show from Num
--------------------------------

With GHC 7.4, one of the first breaking changes (in a while, anyway) was made
to the Prelude - removing the Eq and Show superclass constraints from the Num
class.  Here's how this feature would have made the change non-breaking:

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
class Addable         a where (+)    :: a -> a -> a
class Multiplicable   a where (*)    :: a -> a -> a
class Subtractable    a where (-)    :: a -> a -> a
class Negateable      a where negate :: a -> a
class Absable         a where abs    :: a -> a
class Signumable      a where signum :: a -> a
class FromIntegerable a where fromInteger :: a -> a

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
