---------------------------------------------------------
--
-- Module        : CalcCalendar
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Bank calendar of Haskell Loan.
---------------------------------------------------------
-- | Provides data types and basic functions operating on dates
--   in specific calendars.
module Haslo.CalcCalendar 
                    (module Data.Time
                    ,module Data.Time.Calendar.OrdinalDate
                    ,CalendarType (..)
                    ,diffLoanCalendar
                    ,addMonth
                    ,addMonths
                    ,addYears
                    ,setDay
                    ,setOrdinalDay
                    ,Freq (..)
                    ,freqPerYear
                    ,addPeriods
                    )
where

import Data.Time
--import Data.Time.Calendar.Julian
import Data.Time.Calendar.OrdinalDate
import Text.PrettyShow

-- | Bank calendar is usually different than real one.
data CalendarType = Y360                 -- ^ year has 360 days, each month 30 days
                  | Y360Specific         -- ^ year has 360 days, each month 30 days (specific version,
                                         --   where diff 15.02 31.01 = 14 [not 15 as expected!])
                  | RealCalendar         -- ^ each month has real number of days
                  deriving (Eq, Show, Enum)

-- | Number of days between two dates in different calendars
diffLoanCalendar :: Day -> Day -> CalendarType -> Int
diffLoanCalendar d1 d2 Y360 = fromIntegral (360 * (yd1 - yd2)) + 30 * (md1 - md2) + min 30 dd1 - min 30 dd2
                 where (yd1,md1,dd1) = toGregorian  d1
                       (yd2,md2,dd2) = toGregorian  d2
diffLoanCalendar d1 d2 Y360Specific = fromIntegral (360 * (yd1 - yd2)) + 30 * (md1 - md2) + dd1 - dd2
                 where (yd1,md1,dd1) = toGregorian  d1
                       (yd2,md2,dd2) = toGregorian  d2
diffLoanCalendar d1 d2 RealCalendar = fromIntegral $ diffDays d1 d2
--diffLoanCalendar _ _ cal  = error $ "Calendar " ++ show cal ++ " is not yet defined"

addMonth date | m == 12   = fromGregorian (y+1) 1 d
              | otherwise = fromGregorian y (m+1) d
    where (y,m,d) = toGregorian date

addMonths :: Integer -> Day -> Day
addMonths n date = fromGregorian (y + ((n+m-1) `div` 12)) (fromIntegral (n+m-1) `mod` 12 + 1) d
    where (y,m',d) = toGregorian date
          m = fromIntegral m'

addYears :: Integer -> Day -> Day
addYears n date = fromGregorian (y+n) m d
    where (y,m,d) = toGregorian date

setDay n date = fromGregorian y m n
    where (y,m,d) = toGregorian date

setOrdinalDay n date = fromOrdinalDate  y n
    where (y,d) = toOrdinalDate date

-- | Frequency.
data Freq = Daily
          | Monthly
          | Yearly
          deriving (Eq, Ord, Show, Enum)
          
instance PrettyShow Freq where
   showWithLen n = showWithLen n . show

freqPerYear Daily   = 365
freqPerYear Monthly = 12
freqPerYear Yearly  = 1

-- | Increases a date by given number of given units.
addPeriods :: Freq -> Integer -> Day -> Day
addPeriods Daily   = addDays
addPeriods Monthly = addMonths
addPeriods Yearly  = addYears

