---------------------------------------------------------
--
-- Module        : Financing
-- Copyright     : Bartosz Wójcik (2010)
-- License       : All rights reserved
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Financing rules.
---------------------------------------------------------

module Financing (initFullPlan
                 ,nextDueDate1
                 ,nextDueDate
                 ,fstInstDate
--                 ,amortizationPlan
                 )
where

import Data.Maybe (fromJust)
import Fee
import BasicType
import Parameters
import CalcConfigurationType (instList
                             ,addPeriods
                             ,ClassicLoan
                             )
import CalcCalendar 

import FinancingType (Tranche (..)
                     ,FullPlan
                     ,FullPlanDay
                     ,FullPlanLine (..)
                     ,IntAdjustment
                     ,fpdCapitalAfter
                     ,fpdLateInterestAfter
                     ,fpdIntAdjAfterAmt
                     ,fpdIntAdjAfter
                     ,fpdAdvInterestAfter
                     ,isINT
                     )
import FinancingConfiguration
import CalcConstructors (Instalment (..)
                        ,InstalmentPlanLine (..)
                        ,InstalmentPlan
                        ,newLoanRIL
                        ,newLoanI
                        ,newInstalmentPlanLine
                        ,newLoanRCtlm
                        )
import Calculator (cE2N
                  )
import ErrorHandling
import Control.Monad.Reader

moduleName = "Financing"

-- | Gives first instalment date having finacing date, calendar type, due day and rule name.
fstInstDate :: Freq         -- ^ instalment frequency
            -> CalendarType -- ^ type of loan calendar
            -> FstInstRule  -- ^ Rule of 1st instalment calculation
            -> Int          -- ^ Number of the day in freq period when instalment matures
            -> Day          -- ^ last financing tranche date
            -> Day
fstInstDate Daily _ _ _ date = addDays 1 date
fstInstDate Monthly Y360Specific r n d =  fstInstDate Monthly Y360 r n d
fstInstDate Monthly Y360 FstInstFullFreqAfter n d = fstInstDate Monthly Y360 (FstInstDays 30) n d
fstInstDate Monthly Y360 (FstInstDays i) dueDay date
            | d < dueDay = setDay dueDay newDate
            | otherwise  = setDay dueDay $ addMonths 1 newDate
    where (y,m,d) = toGregorian newDate
          newDate = addDays (fromIntegral i) date
fstInstDate Monthly RealCalendar (FstInstDays i) dueDay date
            | d <= dueDay = fromGregorian y m dueDay
            | otherwise   = fromGregorian y (m+1) dueDay
    where (y,m,d) = toGregorian dateN
          dateN   = addDays (fromIntegral i) date
fstInstDate Monthly RealCalendar FstInstFullFreqAfter dueDay date
            | dn <= dueDay = fromGregorian yn mn dueDay
            | otherwise    = fromGregorian yn (mn+1) dueDay
    where (y,m,d)    = toGregorian date
          dateN      = fromGregorian y (m+1) d
          (yn,mn,dn) = toGregorian dateN
fstInstDate Yearly _ FstInstFullFreqAfter dueDay date
            | d <= dueDay  = fromOrdinalDate (y+1) dueDay
            | otherwise    = fromOrdinalDate (y+2) dueDay
    where (y,d)    = toOrdinalDate date
-- All cases which are not foreseen return just the financing date.
fstInstDate _ _ _ _ d = d

-- | Completes interest amount of the tranche
completeTranche :: FinancingParameters
                -> Int           -- ^ 'fin1stInstDay'
                -> Day           -- ^ first instalment date
                -> Day           -- ^ date of tranche
                -> Tranche       -- ^ tranche with populated amount and date
                -> Tranche
completeTranche fp n fstInstD date tr = 
   tr { trNbrDays = diffDays
      , trInt     = fstInstIntDelta fp (trAmount tr) (trRate tr) $ diffDays
      }
    where -- Number of days between tranche's date and 1st instalment date
          diffDays = diffLoanCalendar fstInstD date (finCalendar fp)

-- | Adjustment of 1st instalment due to different number of days which are between
--   tranche and due date on one hand and usual interest period on the other.
--   For interest calculated daily there is no difference. Function returns 0.
--   If 'FstIntDaily' then there is easy formula 'intFreq' independent.
--   Otherwise formula aasumes certain number of days in usual interest period (30, 360 or 365) depending
--   on the 'intFreq'
fstInstIntDelta :: FinancingParameters
                -> Amount     -- ^ financing amount
                -> Rate       -- ^ yearly effective interest rate
                -> Int        -- ^ number of days of interest accrue
                -> Amount
fstInstIntDelta fp c rE d1
    | intFreq fp == Daily              = 0
    | fin1stIntRule fp == FstIntDaily  = round $ c' * (1 + rD)**d1' - c'
--    | fin1stIntRule fp == FstIntDaily  = round $ c' * (1 + rD)^d1
    | otherwise                        = round $ c' * (1 + r)**(d1' / n' - 1) - c'
--    | otherwise                        = round $ c' * (1 + r)**(fromIntegral d1 / n' - n') - c' * (1 + r)
    where c' = fromIntegral c
          rD = cE2N Daily rE
          r  = cE2N (intFreq fp) rE
          d1' = fromIntegral d1
          n' = fromIntegral n
          n | intFreq fp == Daily            = 1
            | intFreq fp == Monthly          = 30
            | intFreq fp == Yearly &&
              finCalendar fp == RealCalendar = 365
            | otherwise                      = 360

{-
amortizationPlan :: ClassicLoan a
                 => [(Day,Tranche)]       -- | List of scheduled tranches
                 -> Duration              -- | Number of instalments
                 -> Duration              -- | 1st instalment deferrment
                 -> Rate                  -- | Interest rate in yearly effective rate
                 -> Int                   -- | Day of instalment due (for Freq==Daily ignored)
                 -> a                     -- | Classic Loan type
                 -> ParamMonad FullPlan
amortizationPlan tr n d rE day loan = lift (newLoanI loan c n d rE) >>=
                                      initFullPlan day rE tr
    where c = sum $ map (trAmount . snd) tr
    -}

-- | Initialized FullPlan from first till last financing.
initFullPlan :: Int                   -- ^ due day
             -> Rate                  -- ^ yearly effective interest rate
             -> [(Day,Tranche)]       -- ^ list of tranches with dates
             -> InstalmentPlan        -- ^ abstract loan with fee
             -> ParamMonad FullPlan
initFullPlan _ _ [] _ = errMsg moduleName "initFullPlan" "empty tranche"
initFullPlan _ _ _ [] = errMsg moduleName "initFullPlan" "empty instalment plan"
initFullPlan d rE tr absLoan = ask >>= \param ->
    let   -- fp, d and rE are used globaly in all the functions below.
          -- Total capital financed
          fp = fin param
          cap = sum $ map (trAmount . snd) tr
          r | fstIntDaily = cE2N Daily rE
            | otherwise   = cE2N (intFreq fp) rE
          rInst = cE2N (instFreq fp) rE
          -- 1st instalment date
          fstInstD = fstInstDate (instFreq fp) (finCalendar fp) (fin1stInstRule fp) d (fst $ last tr)
          -- Date of 1st inst > 0
          fstInstDPlus = addPeriods (intFreq fp) 
                                    (fromIntegral $ length $ takeWhile (==0) $ map (iAmt . iplInst) absLoan)
                                    fstInstD
          -- Next interest due date > than given date
          nextIntDate date | fstIntDaily = nextDueDate1 Daily d date
                           | otherwise   = nextDueDate1 (intFreq fp) d date
          -- Grace period is counted from first tranche date till
          -- @instFreq - 1@ day before first instalment. Eg: instalment matures
          -- monthly on each 15th, first one on 15th November, first and the only tranche on 
          -- 5th October, then grace period lasts from 6th till 15th October.
          -- @fstUsualDay@ is one day after.
          -- Grace period can be empty. TODO: check: Then @fstUsualDay@ == last tranche date + 1d
          fstUsualDay = addDays 1 $ addPeriods (instFreq fp) (-1) fstInstD
--                        max (addPeriods (instFreq fp) (-1) fstInstD)
--                                        (fst $ last tr)

          fstIntWholePeriod = fin1stIntRule fp == FstIntTill1stInst
          fstIntWholePeriodPlus = fin1stIntRule fp == FstIntTill1stInstGT0
          fstIntCtlm = fin1stIntRule fp == FstIntCtlm
          fstIntDaily = fin1stIntRule fp == FstIntDaily

          -- ============================================
          -- Creates TPL entry of 'FullPlanDay'
          makeTPL :: FullPlanDay        -- previous row in the full instalment plan
                  -> (Day,Tranche)      -- next tranche
                  -> FullPlanDay
          makeTPL fpd (dt,tt) = (dt, TPL newTr cNew iL mIANew)
              where newTr | fstIntWholePeriod ||
                            fstIntWholePeriodPlus ||
                            fstIntCtlm            = completeTranche fp d fstInstD dt tt
                            -- Daily interest in grace period need interest adjustment only if
                            -- first instalment is scheduled not full intrest period after last tranche.
                          | fstIntDaily &&
                            fstUsualDay < dt      = completeTranche fp d fstUsualDay dt tt
                          | fstIntDaily           = completeTranche fp d dt dt tt
                          | otherwise             = completeTranche fp d (nextIntDate dt) dt tt
                    cNew = trAmount newTr + fpdCapitalAfter fpd
                    iL = fpdLateInterestAfter fpd -- + trIntLate newTr
                    mIANew | trInt newTr == 0 &&
                             iAA == 0              = Nothing
--                           | fstIntWholePeriod     = Just (fstInstD,trInt newTr + iAA)
                           | fstIntWholePeriodPlus ||
                             fstIntCtlm            = Just (fstInstDPlus,trInt newTr + iAA)
                           | otherwise             = Just (fstInstD,trInt newTr + iAA)
--                           | otherwise             = Just (nextIntDate dt,trInt newTr + iAA)
                    iAA | mIA == Nothing = 0
                        | otherwise      = snd $ fromJust mIA
                    mIA = fpdIntAdjAfter fpd

          -- ============================================
          -- Creates instalment entry (INT) into FullPlan.
          makeINT :: FullPlanDay
                  -> FullPlanDay
          makeINT fpd = (nextIntD, INT mIA newIPL)
              where -- Usage of laizyness: iAD and iAA are computable only if mIA /= Nothing
                    mIA = fpdIntAdjAfter fpd
                    iL = fpdLateInterestAfter fpd
                    c = fpdCapitalAfter fpd
                    newIPL = newInstalmentPlanLine c iL r i
                    date = fst fpd
                    nextIntD = nextIntDate date
                    nextInstD = nextDueDate1 (instFreq fp) d date
                    -- Instalment amount covers interest or ==0.
                    i | fin1stIntRule fp == FstIntTillNextDue &&
                        nextInstD == nextIntD                    = (-1)
                      | otherwise                                = 0

          -- =============================================
          -- Creates tranche entry (TPL) or interest entry (INT) into FullPlan.
          -- TPL is created on date of tranche, INT on all due dates between tranches
          -- and after last tranche and before first instalment.
          -- Capital after and late interest have to be passed always to the next entry.
          initFullPlan' :: FullPlanDay          -- Previous row of full plan 
                        -> [(Day,Tranche)]      -- list of tranches with dates
                        -> FullPlan
          initFullPlan' fpd []
                -- After last tranche the treatment till first instalment is the same like during
                -- tranches' phase. At this moment it is ensured that @regularPeriod iL@ is @Just x@ value,
                -- so @fromJust@ is safe.
              | mIA /= Nothing && date == iAD   = newAPL : initFullPlan' newAPL []
              | fstIntWholePeriod ||
                fstIntWholePeriodPlus ||
                nextIntDate date >= fstUsualDay = []
              | otherwise                       = newINT : initFullPlan' newINT []
              where newINT = makeINT fpd 
                    newAPL = makeAPL fpd
                    date = fst fpd
                    (iAD,iAA) = fromJust mIA
                    mIA = fpdIntAdjAfter fpd

          initFullPlan' fpd (t:ts)
                -- Next tranche date is before next interest due date
                -- or interest for 1st period are calculated for the whole period at once (then
                -- there is no additional interest calculation before last tranche).
              | mIA /= Nothing && 
                date == iAD                 = newAPL : initFullPlan' newAPL (t:ts)
              | nextIntDate date > fst t ||
                fstIntDaily && fstUsualDay <= nextIntDate date ||
--                fstIntDaily && fstUsualDay < fst t ||
                fstIntWholePeriod ||
                fstIntWholePeriodPlus       = newTPL : initFullPlan' newTPL ts
              | otherwise                   = newINT : initFullPlan' newINT (t:ts)
              where newTPL = makeTPL fpd t
                    newINT = makeINT fpd
                    newAPL = makeAPL fpd
                    date = fst fpd
                    (iAD,iAA) = fromJust mIA
                    mIA = fpdIntAdjAfter fpd
                    
          -- =============================================
          -- Proprietary version of initFullPlan'
          -- Exactly one tranche
          -- Exactly one element in the output
          initFullPlanCtlm :: FullPlanDay          -- Previous row of full plan
                           -> [(Day,Tranche)]      -- list of tranches with dates
                           -> ValidMonad FullPlan
          initFullPlanCtlm fpd [t] = return $ [makeTPL fpd t]
          initFullPlanCtlm fpd ts  = errMsg moduleName "initFullPlanCtlm" $
                                            "unexpected multi tranche case: " ++ show ts
                                            
          dummyFPD = (fst $ head tr, APL 0 0 0)
    in case fin1stIntRule fp of
           FstIntCtlm -> lift $ lift $ initFullPlanCtlm dummyFPD tr >>=
                         regularPeriodCtlm cap absLoan fstInstD
           _          -> lift $ lift $ regularPeriod fp cap absLoan fstInstD fstUsualDay $
                         initFullPlan' dummyFPD tr

-- initFullPlan ===================================================

          -- =============================================
          -- Initiates the regular period of the loan. To be called after all tranches are planed.
          -- Recalculation of interest rate having list of instalments in frequency of interest maturity.
          -- I.e. it takes 'InstalmentPlan' value in frequency of instalment and assigns instalment dates to them
          -- additionally, if necessary, adds additional 'InstalmentPlanLine' entries into the list 
          -- which entires reflect interest maturity events (hence interest maturity frequency) (which have 
          -- instalment amount fixed to 0). 
regularPeriod :: FinancingParameters
              -> Amount              -- ^ capital
              -> InstalmentPlan      -- ^ abstract loan, without dates and tranches
              -> Day                 -- ^ first instalment date
              -> Day                 -- ^ first day of usual period
              -> FullPlan            -- ^ entries of first period
              -> ValidMonad FullPlan
regularPeriod fp cap ip fstInstD fstUsualDate fpl
   | intFreq fp > instFreq fp = errMsg moduleName "regularPeriod" $
                                                  "interest frequency > instalment frequency " ++
                                                  show (intFreq fp) ++ ">" ++ show (instFreq fp)
   | otherwise                = newLoan >>= newIP >>= adjustRegularPeriod mIA
    where -- At this point it's ensured that absLoan is Just x alternative.
          -- List of instalment amounts retrived from InstalmentPlan and enriched with late interest
          -- of grace period.
          -- At the moment late intertest of grace period does not accrue interest!
          is | lateIntRule fp == LateIntFstInst     = head xs + round iL : tail xs
             | lateIntRule fp == LateIntFstSignInst = zs ++ [head ys + round iL] ++ tail ys
             | lateIntRule fp == LateIntLastInst    = init xs ++ [last xs + round iL]
             | otherwise                            = xs
             where xs = instList ip
                   (zs,ys) = span (==0) xs
          nNew = fromIntegral $ length is
          -- List of dates of all instalments
          ds = map (\i -> addPeriods (instFreq fp) i fstInstD) [0 .. (nNew-1)]
          -- List of pairs of instalment's dates and amounts.
          -- Completed by 0 amount instalments on dates where interest matures but there is no instalment due.
          dis | intFreq fp == instFreq fp = zip ds is
              | otherwise                 = dis' fstUsualDate ds is
              where dis' _ [] _ = []
                    dis' date (d:ds) (i:is) | date >= d = (d, i) : dis' nextDate ds is
                                     -- Additional INT entry which reflects interest
                                     -- maturity - hence instalment amount 0.
                                            | otherwise = (date, 0) : dis' nextDate (d:ds) (i:is)
                        where nextDate = addPeriods (intFreq fp) 1 date
          newDs | intFreq fp == instFreq fp = ds
                | otherwise                 = fst $ unzip dis
          -- | Last line of grace period
          lastFPL = last fpl
          -- Late interest after first period
          iL = fpdLateInterestAfter lastFPL
          -- Advanced interest
          iA = fpdAdvInterestAfter lastFPL
          -- At this point nominal interest rate gets recalculated.
          newLoan | intFreq fp == instFreq fp = newLoanRIL (fromIntegral iA + iL) cap is
                  | otherwise                 = newLoanRIL (fromIntegral iA + iL) cap (snd $ unzip dis)
          mIA = fpdIntAdjAfter $ last fpl
          newIP ip' = Right $ fpl ++ zip newDs (map (INT Nothing) ip')
                     
-- | Interest adjustment comming from 1st instalment can be due within
--   regular period. There is max. one adjustment.
adjustRegularPeriod :: Maybe IntAdjustment -- ^ interest adjustment
                    -> FullPlan
                    -> ValidMonad FullPlan
adjustRegularPeriod Nothing fp = return fp
adjustRegularPeriod (Just (iAD,iAA)) fp = return $ adjInst iAD iAA fp
    where adjInst iAD iAA (x:xs) | iAD > d   = xNew : adjInst iAD iAA xs
                                 | iAD == d  = xNew : makeAPL xNew : xs
                                 | iAD < d   = makeAPL xNew : xNew : xs
              where d = fst x                                 
                    fpl = snd x
                    xNew | isINT fpl && intIntAdj fpl == Nothing = (d, fpl {intIntAdj = Just (iAD,iAA)}) 
                         | otherwise                             = x

-- | Reworks regural period recalculating first instalment amount,
--   first instalment interest rate and interest rate of the rest of the loan.
regularPeriodCtlm :: Amount
                  -> InstalmentPlan
                  -> Day
                  -> FullPlan
                  -> ValidMonad FullPlan
regularPeriodCtlm cap ip fstInstD fpl = 
    newLoanRCtlm cap isNew d1 >>= \loan -> (return $
                                            fpl ++ zip ds (map (INT Nothing) loan))
    where fstInst = head isFull + (iAA $ fpdIntAdjAfter fpd)
          is = instList ip
          (isNull,isFull) = span (==0) is
          iAA Nothing = 0
          iAA (Just iA) = snd iA
          -- fst instalment gets recalculated
          isNew = isNull ++ fstInst : tail isFull
          -- At this point nominal interest rate gets recalculated.
          n = fromIntegral $ length is
          -- List of dates of all instalments
          ds = map (\i -> addPeriods Monthly i fstInstD) [0 .. (n-1)]
          d1 = (trNbrDays . tplTranche . snd) fpd
          fpd = head fpl
          

-- ============================================
-- Creates APL entry of 'FullPlanDay'
-- Date == date of last @FullPlanDay@ entry.
-- Capital after and late interest after don't change and are taken from previous entry.
-- Amount of adjustment is taken from carried @IntAdjustment@.
makeAPL :: FullPlanDay        -- previous row in the full instalment plan
        -> FullPlanDay
makeAPL fpd = (fst fpd, APL cAfter iL iAA)
    where cAfter = fpdCapitalAfter fpd
          iL = fpdLateInterestAfter fpd
          iAA = fpdIntAdjAfterAmt fpd                     


-- | Next due date >= than current one.
nextDueDate :: Freq  -- ^ Instalment frequency
            -> Int   -- ^ due day (ignored when @freq == Daily@)
            -> Day   -- ^ tranche day
            -> Day
nextDueDate Daily  _ date = date
nextDueDate freq day date = nextDueDate' freq day date

-- | Next due date > than current one.
nextDueDate1 :: Freq  -- ^ Instalment frequency
             -> Int   -- ^ due day (ignored when @freq == Daily@)
             -> Day   -- ^ tranche day
             -> Day
nextDueDate1 Daily  _ date = addDays 1 date
nextDueDate1 freq day date = nextDueDate' freq day $ addDays 1 date

-- Next due date >= than current one.
-- Always to be called by one of nextDueDate functions.
nextDueDate' Monthly day date | day >= d  = fromGregorian y m day
                              | otherwise = addMonths 1 $ fromGregorian y m day
    where (y,m,d) = toGregorian date
nextDueDate' Yearly day date | day >= d  = fromOrdinalDate y day
                             | otherwise = fromOrdinalDate (y+1) day
    where (y,d) = toOrdinalDate date
nextDueDate' freq _ _ = error $ "Incorrect input " ++ show freq


