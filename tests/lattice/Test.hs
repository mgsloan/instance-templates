{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, RankNTypes, ConstraintKinds, FlexibleContexts #-}
module Test where

import Enumerable
import Lattices

import Language.Haskell.InstanceTemplates
import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

import qualified Data.Set as S

{- Instance templates syntax
deriving class Ord a => MonoidDiffLattice S.Set where
    join = S.union
    meet = S.intersection
    bottom = S.empty

    top :: Enumerable a => S.Set a
    top = S.fromList universe

    diff = S.difference
-}

$(instantiate
 [ template BoundedUniverse_Template [t| BoundedUniverse  Bool |] [d| |]
 , template BoundedUniverse_Template [t| BoundedUniverse  Int  |] [d| |]

 , template Lattice_Template [t| Ord a => Lattice (S.Set a) |] [d|
    join = S.union
    meet = S.intersection
   |]
 ])

instance Ord a => BoundedJoinSemiLattice (S.Set a) where
    bottom = S.empty

instance (Ord a, Enumerable a) => BoundedMeetSemiLattice (S.Set a) where
    top = S.fromList universe

-- TODO: merge all of these into one "instantiate" invocation, once we apply
-- to regular classes

$(instantiate
 [ template MonoidDiffLattice_Template [t| Ord a => MonoidDiffLattice (S.Set a) |] [d|
    diff = S.difference
   |]

 , template BoundedLattice_Template [t| forall a b. BoundedLattice (a, b) |] [d|
    join :: (JoinSemiLattice a, JoinSemiLattice b) => (a, b) -> (a, b) -> (a, b)
    (x1, y1) `join` (x2, y2) = (x1 `join` x2, y1 `join` y2)

    meet :: (MeetSemiLattice a, MeetSemiLattice b) => (a, b) -> (a, b) -> (a, b)
    (x1, y1) `meet` (x2, y2) = (x1 `meet` x2, y1 `meet` y2)

    bottom :: (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => (a, b)
    bottom = (bottom, bottom)

    top :: (BoundedMeetSemiLattice a, BoundedMeetSemiLattice b) => (a, b)
    top = (top, top)
   |]
 ])

instance (BooleanLattice a, BooleanLattice b) => BooleanLattice (a, b) where
  complement (x, y) = (complement x, complement y)

instance (ResiduatedLattice a, ResiduatedLattice b) => ResiduatedLattice (a, b) where
  residualL (x1, y1) (x2, y2) = (residualL x1 x2, residualL y1 y2)
  residualR (x1, y1) (x2, y2) = (residualR x1 x2, residualR y1 y2)

instance (DiffLattice a, DiffLattice b) => DiffLattice (a, b) where
  diff (x1, y1) (x2, y2) = (diff x1 x2, diff y1 y2)