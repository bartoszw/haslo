---------------------------------------------------------
--
-- Module        : ErrorHandling
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Error handling data structures, functions, etc.
---------------------------------------------------------
-- | Provides data types and basic functions allowing better and direct error handling.
module Haslo.ErrorHandling (module Control.Monad.Error
                     ,module Haslo.BasicType
                     ,ValidationError (..)
                     ,ValidMonad
                     )
where

import Control.Monad.Error.Class
import Control.Monad.Error
import Haslo.BasicType
import Haslo.InstalmentPlan
import Text.PrettyShow
import Data.Time (Day
                 ,fromGregorian)

-- | Error handlig data type.
data ValidationError =
             -- | Instalment discrepancy:
             --   Instalment amount, Repayment, Interest paid
               InstalmentDiscrepancy !Amount !Amount !Amount 

             -- | Incorect interest:
             --   Principal before, Late interest before, Interest calculated (incorectly),
             --   Interest rate
             | IPLInterest !Amount !Interest !Interest !Rate

             -- | Simple follow up interest mismatch (a1 + a2 - a3 /= a4):
             --   Error description, a1, a2, a3, a4
             | FollowUpInterestError String !Interest !Interest !Amount !Interest 

             -- | Simple follow up mismatch (a1 - a /= a2):
             --   Error description, a1, a, a2
             | FollowUpError String !Amount !Amount !Amount

             -- | Simpler follow up mismatch (a /= b):
             --   Error description, a, b
             | SimplerFollowUpError String !Amount !Amount

             -- | Deffered interest cannot be paid.
             | NotPaidDefferedInterest !Interest InstalmentPlan

             -- | Capital doesn't amortize. This is due to rounding error if happens.
             | NotAmortized !Amount InstalmentPlan

             | OtherError String

             --  Any error can occur independently before and after financing.
             --   Errors after financing usually are linked to date.
--             | FinancingError Day ValidationError

-- | We make ValidationError an instance of the Error class
--   to be able to throw it as an exception.
instance Error ValidationError where
  noMsg    = OtherError "(!)"
  strMsg s = OtherError s

instance Show ValidationError where
    show (InstalmentDiscrepancy a p iP) = "Instalment discrepancy:\
                     \instalment amount /= repayment + interest paid ("
                          ++ (showAmtWithLen 8 a) ++ "/=" ++
                             (showAmtWithLen 8 p) ++ "+" ++
                             (showAmtWithLen 8 iP) ++ ")"
    show (IPLInterest p iL i r) = "IPL interest discrepancy: \
                          \ recalculated interest=" ++ show iR ++
                          ", given interest=" ++ show i ++
                          " (principal:" ++ showAmtWithLen 8 p ++
                          " (late interest:" ++ showWithLenDec 11 6 iL ++
                          " (interest rate:" ++ show r ++ ")"
        where iR = (fromIntegral p + iL) * r
    show (FollowUpInterestError msg a1 a2 a3 a4) = msg ++
                              show (a1 / 100) ++ " - " ++
                              show (a2 / 100) ++ " + " ++
                              showAmtWithLen 8 a3 ++ " /= " ++
                              show (a4 / 100)
    show (FollowUpError msg a1 a a2) = msg ++
                              showAmtWithLen 8 a1 ++ " - " ++
                              showAmtWithLen 8 a ++ " /= " ++
                              showAmtWithLen 8 a2
    show (SimplerFollowUpError msg a b) = msg ++
                              showAmtWithLen 8 a ++ " /= " ++
                              showAmtWithLen 8 b
    show (NotPaidDefferedInterest iL ip) = "Deferred interest not paid off. Remains:" ++
                                           show (iL / 100) ++
                                           " " ++ show ip
    show (NotAmortized c ip) = "Principal doesn't amortize fully. Remains:" ++ showAmtWithLen 8 c ++
                               " " ++ show ip
    show (OtherError msg) = msg
--    show (FinancingError d err) = show d ++ " " ++ show err

maybeShow Nothing = ""
maybeShow (Just a) = show a ++ " "

-- | Monad wrapping error message or correct value. Broadly used.
type ValidMonad = Either ValidationError


--errMsg moduleName functionName msg = throwError $ OtherError $
--    moduleName ++ " " ++
--    functionName ++ " " ++
--    msg

{-errDate :: Day -> ValidMonad a -> ValidMonad a
errDate d (Left (FollowUpError w x y z _)) = Left $ FollowUpError w x y z (Just d)
errDate d (Left (InstalmentDiscrepancy w x y z _)) = Left $ InstalmentDiscrepancy w x y z (Just d)
errDate d (Left (IPLInterest w x y z _)) = Left $ IPLInterest w x y z (Just d)
errDate _ x                                = x-}

--data FinancingError = SFinancingError Day ValidationError

-- | Allows adding date to any error of @ValidationError@ type.
--liftDate :: MonadError ValidationError m => Day
--                                         -> ValidationError
--                                         -> m a
--liftDate date err = throwError $ FinancingError date err

