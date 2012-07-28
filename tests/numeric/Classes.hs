{-# LANGUAGE
    TemplateHaskell
  , ConstraintKinds
  , ScopedTypeVariables
  , FlexibleInstances
  , MultiParamTypeClasses
  , RebindableSyntax
  #-}

-- |
module Classes where

import Prelude ( ($), (.), (>>), (>=) )
import qualified Prelude as P

import Language.Haskell.InstanceTemplates

import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

class Addable         a where (+)         :: a -> a -> a
class Multiplicable   a where (*)         :: a -> a -> a
class Subtractable    a where (-)         :: a -> a -> a
class Negateable      a where negate      :: a -> a
class Absable         a where abs         :: a -> a
class Signumable      a where signum      :: a -> a
class FromIntegerable a where fromInteger :: P.Integer -> a

-- Necessary due to RebindableSyntax.
instance FromIntegerable P.Int where fromInteger = P.fromInteger


type Num a = (Addable a, Multiplicable a, Subtractable a, Negateable a,
              Absable a, Signumable a, FromIntegerable a)

-- This instance template provides the same interface as the original Num class
-- (except for defaults, which should be supported)
$(mkTemplate P.=<< [d|
  class Num a where

  -- This means that the methods of these classes are used as parameters, and
  -- are provided to the generated instances.
  instance Inherit (Instance
    ( Addable a, Multiplicable a, Subtractable a, Negateable a
    , Absable a, Signumable a, FromIntegerable a )) where

-- Impossible due to TH barfing on reifying Constraint Types
-- (but this ought to work instead)
--  instance Inherit (Methods (Num a))
 |])


type OldNum a = Num a

-- TODO: should there be a shorthand for this variety of template?
$(mkTemplate P.=<< [d|
  class P.Num a => OldNum a where

  instance Addable         a where (+)         = (P.+)
  instance Multiplicable   a where (*)         = (P.*)
  instance Subtractable    a where (-)         = (P.-)
  instance Negateable      a where negate      = P.negate
  instance Absable         a where abs         = P.abs
  instance Signumable      a where signum      = P.signum
  instance FromIntegerable a where fromInteger = P.fromInteger
 |])


-- This provides the boilerplate derivation of these numeric instances,
-- if you have a bijection from your type to a type that supports Num.
-- This will be more concise once instance templates can invoke other
-- templates.

data Bij a b = Bij
  { fwd :: (a -> b)
  , bwd :: (b -> a)
  }

binBij :: Bij a b -> (a -> a -> a) -> (b -> b -> b)
binBij b f x y = fwd b $ f (bwd b x) (bwd b y)

inBij  :: Bij a b ->      (a -> a) ->      (b -> b)
inBij b f = fwd b . f . bwd b

type BijNum a b = Num b

$(mkTemplate P.=<< [d|
  class P.Num a => BijNum a b where
    bij :: Bij a b

  instance Addable         b where (+)         = binBij bij (P.+)
  instance Multiplicable   b where (*)         = binBij bij (P.*)
  instance Subtractable    b where (-)         = binBij bij (P.-)
  instance Negateable      b where negate      =  inBij bij P.negate
  instance Absable         b where abs         =  inBij bij P.abs
  instance Signumable      b where signum      =  inBij bij P.signum
  instance FromIntegerable b where fromInteger = fwd bij . P.fromInteger
 |])
