{-# LANGUAGE InstanceTemplates, RebindableSyntax, MultiParamTypeClasses #-}

-- This is a sketch of what this code would look like with -XInstanceTemplates

import Prelude ( ($), (.), (>>), (>=) )
import qualified Prelude as P

class Addable         a where (+)         :: a -> a -> a
class Multiplicable   a where (*)         :: a -> a -> a
class Subtractable    a where (-)         :: a -> a -> a
class Negateable      a where negate      :: a -> a
class Absable         a where abs         :: a -> a
class Signumable      a where signum      :: a -> a
class FromIntegerable a where fromInteger :: P.Integer -> a

instance FromIntegerable P.Int where fromInteger = P.fromInteger

-- This instance template provides the same interface as the original Num class
-- (except for defaults, which should be supported)
deriving class Num a where
  instance ( Addable a, Multiplicable a, Subtractable a, Negateable a
           , Absable a, Signumable a, FromIntegerable a )



-- This instance template allows you to use a single declaration to generate
-- a 
deriving class P.Num a => OldNum a where
  instance Addable         a where (+)         = (P.+)
  instance Multiplicable   a where (*)         = (P.*)
  instance Subtractable    a where (-)         = (P.-)
  instance Negateable      a where negate      = P.negate
  instance Absable         a where abs         = P.abs
  instance Signumable      a where signum      = P.signum
  instance FromIntegerable a where fromInteger = P.fromInteger


-- This provides the boilerplate derivation of these numeric instances,
-- if you have a bijection from your type to a type that supports Num.

data Bij a b = Bij
  { fwd :: (a -> b)
  , bwd :: (b -> a)
  }

binBij :: Bij a b -> (a -> a -> a) -> (b -> b -> b)
binBij b f x y = fwd b $ f (bwd b x) (bwd b y)

inBij  :: Bij a b ->      (a -> a) ->      (b -> b)
inBij b f = fwd b . f . bwd b

type BijNum a b = Num b


deriving class P.Num a => BijNum a b where
  bij :: Bij a b

  -- Note! This is invoking the template above.
  instance Num a where
    (+)         = binBij bij (P.+)
    (*)         = binBij bij (P.*)
    (-)         = binBij bij (P.-)
    negate      =  inBij bij P.negate
    abs         =  inBij bij P.abs
    signum      =  inBij bij P.signum
    fromInteger = fwd bij . P.fromInteger


-- Usage

instance OldNum P.Double where

newtype Nat = Nat Int
  deriving P.Show

instance BijNum P.Int Nat where
  bij = Bij (Nat . P.max 0) (\(Nat i) -> i)




-- What if we had a more advanced, probably ill-founded, extension?

coercion class (c a) => BijCoerced c a b where
  bij :: Bij a b

  instance c a b using
    fwd bij :: a -> b
    bwd bij :: b -> a

instance BijCoerced Num P.Int Nat where
  bij = Bij (Nat . P.max 0) (\(Nat i) -> i)


-- Could get even more convenience by being able to omit newtype wrapping:

instance Newtype Nat P.Int where
  pack        x  = (Nat x)
  unpack (Nat x) =      x


coercion class BijCoerced2 cs a b where
  bij :: Bij a b

  instance cs using
    fwd bij :: a -> b
    bwd bij :: b -> a
    unpack :: Newtype x y => x -> y
    pack   :: Newtype x y => y -> x

instance BijCoerced2 Num P.Int Nat where
  bij = Bij (P.max 0) id




