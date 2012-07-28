{-# LANGUAGE
    TemplateHaskell
  , ConstraintKinds
  , ScopedTypeVariables
  , FlexibleInstances
  , FlexibleContexts #-}

module Test where

import Classes
import Prelude ( Int, (>>), (.) )
import qualified Prelude as P
import Language.Haskell.InstanceTemplates

import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

$(instantiate [ template OldNum_T [t| OldNum P.Double |] [d| |] ])

newtype Nat = Nat Int
  deriving P.Show

$(instantiate
  [ template BijNum_T [t| BijNum P.Int Nat |] [d|
      bij = Bij (Nat . P.max 0) (\(Nat i) -> i)
     |]
  ])