---------------------------------------------------------
--
-- Module        : Parameters
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Parameters hidden in the reader monad
---------------------------------------------------------

module Haslo.Parameters (IPPMonad
                  ,runWithIPP
                  ,InstalmentPlanParam (..)
                  ,paramDT
                  ,paramMT
                  ,paramYT
                  ,paramDR
                  ,paramMR
                  ,paramYR
                  )
where

import Control.Monad.Reader
import Haslo.ErrorHandling
import Haslo.CalcCalendar 

type ParamMonad = ReaderT Param IPPMonad

runWithParam param loan = runReaderT loan param

data Param = Param
     {ipp  :: InstalmentPlanParam
     ,fin  :: FinancingParameters
     }
     deriving Show

-- | Parameters for loan construction. They have been separated, because they 
--   usually do not change often, if at all.
data InstalmentPlanParam = IPP
     {freq        :: Freq
     ,rounding    :: RoundingType
     }
     deriving Show
     
-- | Reader monad for IPP -- to run loan with parameters.
type IPPMonad = ReaderT InstalmentPlanParam ValidMonad

-- | Convenience function.
runWithIPP :: InstalmentPlanParam -> IPPMonad a -> ValidMonad a
runWithIPP param loan = runReaderT loan param
     
absParamMonthlyTrunc = IPP Monthly Truncated
absParamDailyTrunc = IPP Daily Truncated
absParamYearlyTrunc = IPP Yearly Truncated

-- | Rule of 1st instalment date as function of last financing tranche's date.
data FstInstRule = FstInstDays Int      -- ^ Not less than given number of days after.
                 | FstInstFullFreqAfter -- ^ at least one full 'instFreq' period after, less than two periods.
                 deriving (Eq,Show)

instance Enum FstInstRule where
   toEnum 0 = FstInstFullFreqAfter
   toEnum n = FstInstDays n

   fromEnum FstInstFullFreqAfter  = 0
   fromEnum (FstInstDays n)       = n

-- | Rule of calculation and maturity of interest for first period of the loan
--   from 1st tranche date till one instalment period before first instalment.
data FstIntRule =

   -- | Interest is calculated daily.
   --   Mature on each instalment day.
     FstIntDaily

   -- | Interest is calculated from tranche date till next instalment day, if next instalment
   --   day is before first instalment date.
   --   Interest matures on each instalment day.
   | FstIntTillNextDue

   -- | Interest is calculated from tranche date till first instalment date.
   --   Interest matures on first instalment date.
   | FstIntTill1stInst

   -- | Interest is calculated from tranche date till first instalment date
   --   which amount > 0. All instalments = 0 before have to be removed. Date of 
   --   first >0 instalment doesn't change though.
   --   Interest matures on date of first >0 instalment.
   | FstIntTill1stInstGT0
   
   -- | Proprietary solution.
   --   Interest is calculated and matures like for @FstIntTill1stInstGT0@, the difference
   --   is that common nominal interest rate is re-caluclated using real number of days of
   --   between tranche date and first instalment date. In this solution only one tranche is 
   --   alowed.
   | FstIntCtlm
   deriving (Eq,Show,Enum)

-- | Rule defining what to do with late interest coming out of grace period.
data LateIntOfGracePeriodRule =

     -- | Late interest will be due on first instalment - will increase its amount.
     LateIntFstInst

     -- | Late interest will be due on first significant instalment - will increase its amount.
   | LateIntFstSignInst

     -- | Late interest will be due on last instalment - will increase its amount.
   | LateIntLastInst
   deriving (Eq,Show)

-- | Collection of parameters customizing financing.
data FinancingParameters = FP {
                      instFreq         :: Freq           -- ^ Frequency of instalment must be >= than @intFreq@
                    , intFreq          :: Freq           -- ^ Frequency of interest
                    , fin1stInstRule   :: FstInstRule    
                    , fin1stIntRule    :: FstIntRule     
                    , finCalendar      :: CalendarType   -- ^ calendar type
                    , lateIntRule      :: LateIntOfGracePeriodRule
                    }
                 deriving (Eq,Show)

paramDT = IPP Daily Truncated
-- | Monthly Truncated
paramMT :: InstalmentPlanParam
paramMT = IPP Monthly Truncated
paramYT = IPP Yearly Truncated
paramDR = IPP Daily Truncated
paramMR = IPP Monthly Rounded
-- | Yearly Rounded
paramYR :: InstalmentPlanParam
paramYR = IPP Yearly Rounded
