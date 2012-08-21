---------------------------------------------------------
--
-- Module        : Sample
-- Copyright     : Bartosz Wójcik (2011)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Part of haslo. Quick Check sample test cases.
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

main = putStrLn "Product            \
                \Princip. \
                \Dur \
                \De \
                \Rate     \
                \(Balloon)  \
                \Parameters" >>
       sample (arbitrary :: Gen TestL) 
