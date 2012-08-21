---------------------------------------------------------
--
-- Module        : Haslo
-- Copyright     : Bartosz Wójcik (2012)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- | Haslo aplication interface. The only module from haslo library you need to import.
---------------------------------------------------------

module Haslo
                (module Haslo.Parameters
                ,module Haslo.ErrorHandling
                ,module Haslo.InstalmentPlan
                ,module Haslo.InstalmentPlanProps
                ,module Haslo.CalcConfigurationType
                ,module Haslo.CalcConstructors
                ,module Haslo.Calculator
                ,getVersionHaslo
                )
where

import Haslo.Parameters
import Haslo.ErrorHandling
import Haslo.InstalmentPlan
import Haslo.InstalmentPlanProps
import Haslo.CalcConfigurationType
import Haslo.CalcConstructors
import Haslo.Calculator

-- | Human friendly formatet output of list wrapped in the Either.
--   Usefull for quick validation of loan constructors.
niceShow :: (Show a, Show a1) => Either a1 [a] -> IO ()
niceShow (Right xs) = mapM_ (putStrLn . show) xs
niceShow (Left err) = putStrLn $ show err

getVersionHaslo = "0.1"
