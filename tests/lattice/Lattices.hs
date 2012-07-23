{-# LANGUAGE TemplateHaskell, QuasiQuotes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances, FlexibleContexts,
    UndecidableInstances #-} -- Not actually using this, just necessary for an AST quote

-- From Max Bolingbroke's lattices library
module Lattices where

import Data.Monoid (Monoid)

import Language.Haskell.InstanceTemplates
import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

-- | A algebraic structure with element joins: <http://en.wikipedia.org/wiki/Semilattice>
--
-- Associativity: x `join` (y `join` z) == (x `join` y) `join` z
-- Commutativity: x `join` y == y `join` x
-- Idempotency:   x `join` x == x
--
-- Partial-Order: x `leq` y == (x `join` y == y)
class JoinSemiLattice a where
    join :: a -> a -> a

-- | The partial ordering induced by the join-semilattice structure
joinLeq :: (Eq a, JoinSemiLattice a) => a -> a -> Bool
joinLeq x y = x `join` y == y

-- | The join of at a list of join-semilattice elements (of length at least one)
joins1 :: JoinSemiLattice a => [a] -> a
joins1 = foldr1 join

-- | A algebraic structure with element meets: <http://en.wikipedia.org/wiki/Semilattice>
--
-- Associativity: x `meet` (y `meet` z) == (x `meet` y) `meet` z
-- Commutativity: x `meet` y == y `meet` x
-- Idempotency:   x `meet` x == x
--
-- Partial-Order: x `leq` y == (x `meet` y == x)
class MeetSemiLattice a where
    meet :: a -> a -> a

-- | The partial ordering induced by the meet-semilattice structure
meetLeq :: (Eq a, MeetSemiLattice a) => a -> a -> Bool
meetLeq x y = x `meet` y == x

-- | The meet of at a list of meet-semilattice elements (of length at least one)
meets1 :: MeetSemiLattice a => [a] -> a
meets1 = foldr1 meet

-- | The combination of two semi lattices makes a lattice if the absorption law holds:
-- see <http://en.wikipedia.org/wiki/Absorption_law> and <http://en.wikipedia.org/wiki/Lattice_(order)>
--
-- Absorption: a `join` (a `meet` b) == a `meet` (a `join` b) == a
type Lattice a = (JoinSemiLattice a, MeetSemiLattice a)

$(mkTemplate =<< [d|
  class Lattice a where
  instance Inherit (Instance (JoinSemiLattice a, MeetSemiLattice a)) where
 |])

{- Instance template syntax

deriving class Lattice a where
  instance (JoinSemiLattice a, MeetSemiLattice a)
-}


-- | A join-semilattice with some element |bottom| that `join` approaches.
--
-- Identity: x `join` bottom == x
class JoinSemiLattice a => BoundedJoinSemiLattice a where
    bottom :: a

-- | The join of a list of join-semilattice elements
joins :: BoundedJoinSemiLattice a => [a] -> a
joins = foldr join bottom

-- | A meet-semilattice with some element |top| that `meet` approaches.
--
-- Identity: x `meet` top == x
class MeetSemiLattice a => BoundedMeetSemiLattice a where
    top :: a

-- | The meet of a list of meet-semilattice elements
meets :: BoundedMeetSemiLattice a => [a] -> a
meets = foldr meet top

-- | Lattices with both bounds
--
-- Partial-Order: bottom `leq` top
type BoundedLattice a = (Lattice a, BoundedJoinSemiLattice a, BoundedMeetSemiLattice a)

$(mkTemplate =<< [d|
  class BoundedLattice a where

  instance Inherit (Instance
    ( JoinSemiLattice a, MeetSemiLattice a
    , BoundedJoinSemiLattice a, BoundedMeetSemiLattice a
    )) where
 |])

{- Instance template syntax

deriving class BoundedLattice a where
  instance (Lattice a, BoundedJoinSemiLattice a, BoundedMeetSemiLattice a)
-}

-- | Boolean lattices have a complement and are distributive over join and meet:
--   <http://en.wikipedia.org/wiki/Complemented_lattice>
--
-- Complement Join: complement x `join` x == top
-- Complement Meet: complement x `meet` x == bottom
-- Involution: complement (complement x) == x
-- Order-Reversing: complement x `leq` complement y == y `leq` x
class BoundedLattice a => BooleanLattice a where
    complement :: a -> a

-- | Lattices with residuals for the Monoid: <http://en.wikipedia.org/wiki/Residuated_lattice>
--
-- TODO: MeetSemiLattice variant?
--
-- (y `leq` residueR x z) === (mappend x y `leq` z) === (x `leq` residueL z y)
class (JoinSemiLattice a, Monoid a) => ResiduatedLattice a where
    residualL, residualR :: a -> a -> a

-- | Lattices with implication - Heyting Algebras: <http://en.wikipedia.org/wiki/Heyting_algebra>
-- class (BoundedLattice a => Heyting a) where


--
-- Partial-Order: a `leq` b == (a `diff` b == top)
-- Distributive: a `diff` (b `join` c) == (a `diff` b) `join` (a `diff` c)
-- a `join` (a `diff` b) == a `join` b
-- b `join` (a `diff` b) == b
class (BoundedJoinSemiLattice a, BoundedMeetSemiLattice a) => DiffLattice a where
    diff :: a -> a -> a

-- More issues representing constraint kinds in TH.  The above should be
--   class BoundedLattice a => DiffLattice a where


type MonoidDiffLattice a = (BoundedLattice a, ResiduatedLattice a, DiffLattice a)

$(mkTemplate =<< [d|
  class MonoidDiffLattice a where

  instance Inherit (Instance
    ( JoinSemiLattice a, MeetSemiLattice a
    , BoundedJoinSemiLattice a, BoundedMeetSemiLattice a
    , DiffLattice a
    )) where

  -- When the Monoid operations are the same as the BoundedJoinSemiLattice,
  -- residueL === residueR === diff
  instance ResiduatedLattice a where
    residualL = diff
    residualR = diff
 |])