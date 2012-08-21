---------------------------------------------------------
--
-- Module        : InstalmentPlan
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- | This module implements 'InstalmentPlan' data type.
---------------------------------------------------------
module Haslo.InstalmentPlan 
where
import Haslo.BasicType
import Text.PrettyShow


-- | Single instalment details.
--   Single instalment is always balanced - see @instalmentCheck@.
data Instalment = I { iAmt       :: Amount   -- ^ installment amount
                    , iRepayment :: Amount   -- ^ repayment
                    , iInterest  :: Interest -- ^ interest calculated over the full period
                    , iIntPaid   :: Amount   -- ^ interest paid with installment
                    }
                    deriving (Eq,Ord)

instance Show Instalment where
         show (I i c int iP) = showAmtWithLen 10 i ++ "="
                            ++ showAmtWithLen 10 c ++ "+"
                            ++ showAmtWithLen 8 iP ++ "; "
                            ++ show (int / 100)




-- | Single element of 'InstalmentPlan'.
--   One element of instlment plan contains following ideas:
--   - details of instalment (see above)
--   - principal after instalment has matured
--   - late not yet paid interest (which are base for interest accrual)
--   - interest rate of current instalment
data InstalmentPlanLine = IPL { iplInst      :: Instalment  -- ^ Instalment itself
                              , iplPrincipal :: Amount      -- ^ Principal after
                              , iplIntLate   :: Interest    -- ^ Sum of not paid late interest after current instalment
                              , iplRate      :: Rate        -- ^ Nominal interest rate
                              }
                              deriving (Eq,Ord)

instance Show InstalmentPlanLine where
         show (IPL i c iL r) = "(" ++ show i ++ ") "
                             ++ showAmtWithLen 11 c ++ " "
                             ++ show (iL / 100) ++ " "
                             ++ show r



-- | Plan of instalments reflects whole life of loan. Following rules are implemented:
--
--   * All instalments are regular duration. Duration is not stored within instalment plan.
--
--   * Each instalment can have its own interest rate which is stored within each row separately.
--
--   * Any payments postponements like first intalment postponement are implemented as
--   usual instalment with instalment amount equal 0.
type InstalmentPlan = [InstalmentPlanLine]


-- | Unwraps list of instalment amounts from InstalmentPlan
instList :: InstalmentPlan -> [Amount]
instList = map (iAmt . iplInst)

-- | Initial principal of a loan
initPrincipal :: InstalmentPlan -> Amount
initPrincipal ip =  iplPrincipal ipl + (iRepayment . iplInst) ipl
        where ipl = head ip

-- | Folded instalment plan is another view of the loan.
--   It folds all instalments of same amount and interest rate into one entry of given duration.
type FoldedInstalmentPlan = [FoldedInstalmentPlanLine]

-- | One entry of @FoldedInstalmentPlan@.
data FoldedInstalmentPlanLine = FIPL { fiplAmt  :: !Amount    -- ^ Instalment amount.
                                     , fiplDur  :: !Duration  -- ^ Number of such instalments in this series.
                                     , fiplRate :: !Rate      -- ^ Interest rate of series.
                                     }
     deriving (Eq, Ord, Show)
     
     
-- | Folds @InstalmentPlan@ to @FoldedInstalmentPlan@
foldIP :: InstalmentPlan -> FoldedInstalmentPlan
foldIP [] = []
foldIP (ipl : ip) = foldIP' ip [iplToFipl ipl]
    where iplToFipl (IPL (I a _ _ _) _ _ r) = FIPL a 1 r
          incDuration (FIPL a n r) = FIPL a (n+1) r
          foldIP' [] result = reverse result
          foldIP' (ipl : ip) (x:xs) | a == fiplAmt x && r == fiplRate x = foldIP' ip (incDuration x : xs)
                                    | otherwise                         = foldIP' ip (iplToFipl ipl : x : xs)
              where a = (iAmt $ iplInst ipl)
                    r = (iplRate ipl)
     
