{-# LANGUAGE TemplateHaskell, QuasiQuotes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}

module Test where

import Classes
import qualified Prelude as P
import Language.Haskell.InstanceTemplates

import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

$(instantiate [ template OldNum_Template [t| OldNum P.Int |] [d| |] ])