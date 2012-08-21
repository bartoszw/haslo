---------------------------------------------------------
--
-- Module        : CalcConfigurationType
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- | This module implements data types of loan consturctors.
---------------------------------------------------------
module Haslo.CalcConfigurationType 
                             (module Haslo.CalcCalendar
                             ,InstalmentLoanData (..)
                             ,ClassicLoan (..)
                             ,Balloons (..)
                             ,UnfdBalloons (..)
                             ,Classical (..)
                             ,Balloon (..)
                             ,BalloonPlus (..)
                             ,UnfdBalloon (..)
                             ,UnfdBalloonPlus (..)
                             ,ReversBalloon (..)
                             ,Bullet (..)
                             ,myRound
                              )

where

import Haslo.BasicType
import Haslo.CalcCalendar
import Haslo.ErrorHandling
import Haslo.Parameters
import Haslo.InstalmentPlan
import Text.PrettyShow


-- ==============================
-- Types' definitions
-- ==============================

-- | Data type for retriving loan data of particular products directly from consturctors.
data InstalmentLoanData = InstalmentLoanData {principal  :: Amount
                                             ,duration   :: Duration
                                             ,deferrment :: Duration
                                             ,rate       :: Rate
                                             }
    deriving (Eq, Ord, Show)

-- | Classical Loan provides a constructor function which
--   creates @Instalment Plan@ of this loan.
class ClassicLoan a where
      newLoanI  :: a              -- ^ type of loan
                -> IPPMonad InstalmentPlan
      extract :: a -> InstalmentLoanData

-- | Class of loans with specific last instalment.
class (ClassicLoan a) => Balloons a where
   balloon :: a -> Amount

-- | Class of loans with extended duration.
class (Balloons a) => UnfdBalloons a where
   eXtendedDuration :: a -> Duration  -- ^ Max duration of extended period.

-- | Eeach installment is equal:
--   principal, duration in freq units, 1st instalment postponement in duration units, 
--   yearly effective interest rate
data Classical = Classical !Amount        -- principal
                           !Duration      -- duration in freq units
                           !Duration      -- 1st instalment postponement in duration units
                           !Rate          -- yearly effective interest rate
     deriving (Eq, Ord)

-- | Last installment is given, rest are equal
--   Datatype elements: principal, duration in freq units, 1st instalment postponement in duration units, 
--   yearly effective interest rate,balloon amount.
data Balloon = Balloon !Amount        -- principal
                       !Duration      -- duration in freq units
                       !Duration      -- 1st instalment postponement in duration units
                       !Rate          -- yearly effective interest rate
                       !Amount        -- balloon amount
     deriving (Eq, Ord)

-- | Balloon + given amount is put on top of last installment.
-- | Last installment is given, rest are equal
--   Datatype elements: principal, duration in freq units, 1st instalment postponement in duration units, 
--   yearly effective interest rate,balloon amount.
data BalloonPlus = BalloonPlus !Amount        -- principal
                               !Duration      -- duration in freq units
                               !Duration      -- 1st instalment postponement in duration units
                               !Rate          -- yearly effective interest rate
                               !Amount        -- balloon amount
     deriving (Eq, Ord)

-- | 2 periods product with given duration of 1st one and
--   residual balloon amount (RBA).
--   RBA serves to calculate first period installment amount like in balloon.
--   2nd period amount is equal to 1st one, its duration is calculated
--   based on it. Last installment may differ.
--   Datatype elements: principal, duration in freq units, 1st instalment postponement in duration units, 
--   yearly effective interest rate, residual balloon amount (RBA), maximal duration of unfolded RBA.
data UnfdBalloon = UnfdBalloon !Amount        -- principal
                               !Duration      -- duration till balloon (inclusive) in freq units
                               !Duration      -- 1st instalment postponement in duration units
                               !Rate          -- yearly effective interest rate
                               !Amount        -- residual balloon amount (RBA).
                               !Duration      -- maximal duration of unfolded RBA
     deriving (Eq, Ord)

-- | Similar to UnfdBalloon. There is 1 difference: RBA maturess together with
-- last installment of 1st period (like BalloonPlus),
-- whilst in UnfdBalloon - one month later.
--   Datatype elements: principal, duration in freq units, 1st instalment postponement in duration units, 
--   yearly effective interest rate, residual balloon amount (RBA), maximal duration of unfolded RBA.
data UnfdBalloonPlus = UnfdBalloonPlus !Amount        -- principal
                                       !Duration      -- duration till balloon (inclusive) in freq units
                                       !Duration      -- 1st instalment postponement in duration units
                                       !Rate          -- yearly effective interest rate
                                       !Amount        -- residual balloon amount (RBA).
                                       !Duration      -- maximal duration of unfolded RBA
     deriving (Eq, Ord)

-- | Each installment is equal to given amount but the last one.
--   Datatype elements: principal, duration in freq units, 1st instalment postponement in duration units, 
--   yearly effective interest rate, regular instalment amount.
data ReversBalloon = ReversBalloon !Amount        -- principal
                                   !Duration      -- duration till balloon (inclusive) in freq units
                                   !Duration      -- 1st instalment postponement in duration units
                                   !Rate          -- yearly effective interest rate
                                   !Amount        -- regular instalment amount.
     deriving (Eq, Ord)

-- | Balloon, where all installments equal 0 except last one which contains full
--   capital and first one which contains interest.
--   Datatype elements: principal, duration in freq units, 1st instalment postponement in duration units, 
--   yearly effective interest rate.
data Bullet = Bullet !Amount        -- principal
                     !Duration      -- duration till balloon (inclusive) in freq units
                     !Duration      -- 1st instalment postponement in duration units
                     !Rate          -- yearly effective interest rate
     deriving (Eq, Ord)


instance Show Classical where
   show (Classical p n d r) = showWithLen 20 "Classical" ++
                              showAmtWithLen 11 p ++
                              showWithLen 5 n ++
                              showWithLen 3 d ++ 
                              " " ++ show r
instance Show Balloon where
   show (Balloon p n d r b) = showWithLen 20 "Balloon" ++
                              showAmtWithLen 11 p ++
                              showWithLen 5 n ++
                              showWithLen 3 d ++
                              " " ++ show r ++ " " ++
                              showAmtWithLen 11 b
instance Show BalloonPlus where
   show (BalloonPlus p n d r b) = showWithLen 20 "BalloonPlus" ++
                              showAmtWithLen 11 p ++
                              showWithLen 5 n ++
                              showWithLen 3 d ++
                              " " ++ show r ++ " " ++
                              showAmtWithLen 11 b
instance Show UnfdBalloon where
   show (UnfdBalloon p n d r b x) = showWithLen 20 "UnfoldedBalloon" ++
                              showAmtWithLen 11 p ++
                              showWithLen 5 n ++
                              showWithLen 3 d ++
                              " " ++ show r ++ " " ++
                              showAmtWithLen 11 b ++
                              showWithLen 5 x
instance Show UnfdBalloonPlus where
   show (UnfdBalloonPlus p n d r b x) = showWithLen 20 "UnfoldedBalloonPlus" ++
                              showAmtWithLen 11 p ++
                              showWithLen 5 n ++
                              showWithLen 3 d ++
                              " " ++ show r ++ " " ++
                              showAmtWithLen 11 b ++
                              showWithLen 5 x
instance Show ReversBalloon where
   show (ReversBalloon p n d r b) = showWithLen 20 "ReversBalloon" ++
                              showAmtWithLen 11 p ++
                              showWithLen 5 n ++
                              showWithLen 3 d ++
                              " " ++ show r ++ " " ++
                              showAmtWithLen 11 b
instance Show Bullet where
   show (Bullet p n d r) = showWithLen 20 "Bullet" ++
                              showAmtWithLen 11 p ++
                              showWithLen 5 n ++
                              showWithLen 3 d ++
                              " " ++ show r

-- | Rounding function out of RoundingType.
myRound :: (RealFrac a) => RoundingType -> (a -> Amount)
myRound Rounded    = round
myRound Truncated  = truncate

