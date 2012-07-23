{-# LANGUAGE TemplateHaskell, QuasiQuotes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}
module Classes where

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

type Num a = (Addable a, Multiplicable a, Subtractable a, Negateable a,
              Absable a, Signumable a, FromIntegerable a)

$(mkTemplate P.=<< [d|
  class Num a where
  instance Inherit (Instance
    ( Addable a, Multiplicable a, Subtractable a, Negateable a
    , Absable a, Signumable a, FromIntegerable a )) where

-- Impossible due to TH barfing on reifying Constraint Types
-- (but this ought to work instead)
--  instance Inherit (Methods (Num a))
 |])

{- Instance Templates syntax

deriving class Num a where
  instance ( Addable a, Multiplicable a, Subtractable a, Negateable a
           , Absable a, Signumable a, FromIntegerable a )

-}


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
