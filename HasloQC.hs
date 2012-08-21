---------------------------------------------------------
--
-- Module        : HasloQC
-- Copyright     : Bartosz Wójcik (2012)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Part of haslo. Quick Check of all necessary functions.
---------------------------------------------------------

module Main
where

import Haslo
import Haslo.HasloQCTestL
import Data.IORef
import Data.List
import Data.Time
import Control.Monad.Reader
import Test.QuickCheck


main = do
   putStrLn "Test suite run. It may take up to several minutes time."
   myRun "Checking interest rate" heavyArgs propRate
   myRun "Checking rate conversion" myArgs prop_cEN
   myRun "Checking abstract amortization table" veryHeavyArgs propInstPlan
--   myRun "Checking AMOR T" myArgs propInstPlanM


myRun txt args prop = do
   putStr $ txt ++ " - " ++ show (maxSize args) ++ " random tests"
   putStrLn " ... "
   quickCheckWith args prop

veryHeavyArgs = Args
  { replay     = Nothing
  , maxSuccess = 100000
  , maxDiscard = 7000
  , maxSize    = 100000
  , chatty     = True
-- noShrinking flag?
  }

heavyArgs = Args
  { replay     = Nothing
  , maxSuccess = 10000
  , maxDiscard = 7000
  , maxSize    = 10000
  , chatty     = True
-- noShrinking flag?
  }

myArgs = Args
  { replay     = Nothing
  , maxSuccess = 300
  , maxDiscard = 2500
  , maxSize    = 300
  , chatty     = True
-- noShrinking flag?
  }

patientArgs = Args
  { replay     = Nothing
  , maxSuccess = 300
  , maxDiscard = 10000
  , maxSize    = 300
  , chatty     = True
}
