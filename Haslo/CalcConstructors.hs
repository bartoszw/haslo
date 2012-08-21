                                                                                                                                                                                                                                                ---------------------------------------------------------
--
-- Module        : CalcConstructors
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- | Convenience functions to construct different kinds of abstract loans or 'InstalmentPlan'.
-- -------------------------------------------------------
module Haslo.CalcConstructors 
                        (newLoanR
                        ,newLoanRIL
                        ,newInstalmentPlanLine
                        ,recalculateEffectiveRate
                        )
where

import Haslo.BasicType 
import Haslo.CalcCalendar
import Haslo.CalcConfigurationType 
import Haslo.InstalmentPlan
import Haslo.Calculator
import Haslo.ErrorHandling
import Haslo.Parameters
import Data.Maybe (fromJust)
import Control.Monad.Reader

moduleName = "CalcConstructors"

-- ------ Loan constructors ---------------
-- | Instalment details have some assumptions:
--   late interest accrue interest
--   installment is always in full installment duration
--   there are no gaps between instalments, though instalment amount can be = 0
newInstalment :: Amount        -- ^ capital before installment (excluded late interest)
              -> Rate          -- ^ rate in frequency unit
              -> Amount        -- ^ instalment amount; when < 0 will be forced to interest + late interest
              -> Interest      -- ^ late interest to be paid by the instalment
              -> Instalment    -- ^ full details for intalment
newInstalment c r i lateInterest
   | i < 0 = I interestPaid'
               0
               interest
               interestPaid'
   | otherwise = I i
                   capitalPaid
                   interest
                   interestPaid
    where interest = (fromIntegral c + lateInterest) * r
          interestPaid' = round $ interest + lateInterest
          interestPaid = min i $ round $ interest + lateInterest
          capitalPaid = i - interestPaid 

-- | Instalment details based on next instalment details
--   Constrain: rate >= 0
--   Late interest cannot be recognized.
prevInstalment :: Amount      -- ^ capital after instalment
               -> Rate        -- ^ rate in frequency unit
               -> Amount      -- ^ instalment amount
               -> Instalment
prevInstalment c r i = I i
                         capitalPaid
                         interest
                         interestPaid
    where interest = fromIntegral (c + i) * r / (1 + r)
          interestPaid = round interest
          capitalPaid = i - interestPaid -- constrain for r >=0 !


-- | Creates one line of instalment plan
newInstalmentPlanLine :: Amount              -- ^ capital before excluded late interest
                      -> Interest            -- ^ total late interest before instalment
                      -> Rate                -- ^ nominal rate in frequency units
                      -> Amount              -- ^ instalment amount
                      -> InstalmentPlanLine  -- ^ instalment plan one row
newInstalmentPlanLine c iL r i = IPL inst
                                     (c - iRepayment inst)
                                     (iL + iInterest inst         -- late interest can be negative
                                         - fromIntegral (iIntPaid inst))
                                     r
    where inst = newInstalment c r i iL

-- | Creates one line of instalment plan
prevInstalmentPlanLine :: Amount    -- ^ capital after instalment excluded late interest
                       -> Rate      -- ^ rate in frequency units
                       -> Amount    -- ^ instalment amount
                      -> InstalmentPlanLine  -- ^ instalment plan one row
prevInstalmentPlanLine c r i = IPL inst
                                   c
                                   0
                                   r
    where inst = prevInstalment c r i



-- | Calculates instalment plan for input where interest rate is not known.
--   Specialization of 'newLoanRIL'.
newLoanR :: Amount         -- ^ principal
         -> [Amount]       -- ^ list of instalment amounts
         -> ValidMonad InstalmentPlan
newLoanR = newLoanRIL 0


-- | Calculates instalment loan details for input where interest rate is not known.
--   Checks whether InstalmentPlan amortizes properly. If not throws apropriate error.
--   Amotizes means: no deferred interest remains. Remaining capital is 0.
--   Additionaly allows to move part of interest to the begining of the loan.
--   This feature is usefull for early regulation case, first interest is paid before principal.
newLoanRIL  :: Interest                   -- ^ late interest - amount of interest to be taken before principal.
            -> Amount                     -- ^ capital
            -> [Amount]                   -- ^ list of instalment amounts
            -> ValidMonad InstalmentPlan
newLoanRIL iL c is = rateIrr is (c + round iL) >>= check . newLoanR' c is iL
    where newLoanR' _ [] _ _      = []
          newLoanR' c (i:is') iL' r = newIPL
                                    : newLoanR' (iplPrincipal newIPL) is' (iplIntLate newIPL) r
              where newIPL = newInstalmentPlanLine c iL' r i
          check ip | lastCap /= 0 && r > 0     = throwError $ NotAmortized lastCap ip
                   | lastCap > fromIntegral (length is) && 
                     r == 0                    = throwError $ NotAmortized lastCap ip
                   | abs lastDefInt > 1e-2     = throwError $ NotPaidDefferedInterest lastDefInt ip
                   | otherwise                 = return ip
              where IPL _ lastCap lastDefInt r = last ip

-- | Gives recalculated single effective interest rate of given loan.
recalculateEffectiveRate :: Freq -> InstalmentPlan -> ValidMonad Rate
recalculateEffectiveRate fr ip =  liftM (cN2E fr) $ rateIrr (instList ip) (initPrincipal ip)

-- =============================================================================

-- | Classical Loan: all instalments are equal.
instance ClassicLoan Classical where
    newLoanI (Classical c n d rE) =
        ask >>= \param ->
        let   r = cE2N (freq param) rE
              i = myRound (rounding param) $ rawCalcInstCl (fromIntegral c) n r d
        in lift $ newLoanR c $ replicate d 0 ++ replicate n i

    extract (Classical p n d r) = InstalmentLoanData p n d r

-- | Balloon: last instalment is given, the other are equal.
instance ClassicLoan Balloon where
    newLoanI (Balloon c n d rE b) =
        ask >>= \param ->
        let   r = cE2N (freq param) rE
              i = myRound (rounding param) $ rawCalcInstBal (fromIntegral c) n r d (fromIntegral b)
        in lift $ newLoanR c $ replicate d 0 ++ replicate (n-1) i ++ [b]

    extract (Balloon p n d r _) = InstalmentLoanData p n d r

-- | BalloonPlus: last instalment is equal to normal instalment + given amount.
--   The other instalments are equal.
instance ClassicLoan BalloonPlus where
    newLoanI (BalloonPlus c n d rE b) =
        ask >>= \param ->
        let   r = cE2N (freq param) rE
              i = myRound (rounding param) $ rawCalcInstBalPlus (fromIntegral c) n r d
                                                                      (fromIntegral b)
        in lift $ newLoanR c $ replicate d 0 ++ replicate (n-1) i  ++ [b + i]

    extract (BalloonPlus p n d r _) = InstalmentLoanData p n d r

-- | UnfdBalloon: Loan type based on Balloon type. It differs from Balloon that is unfolds balloon instalment so
--   that all instalments are equal, except the last one which is less or equal.
instance ClassicLoan UnfdBalloon where
    newLoanI (UnfdBalloon c n d rE b x) =
        ask >>= \param ->
        let r = cE2N (freq param) rE
            i = myRound (rounding param) $ rawCalcInstBal (fromIntegral c) n r d (fromIntegral b)
            durOfBal :: Integer
            durOfBal  = calcDurCl (fromIntegral $ calcCapBeforeBal b r) (fromIntegral i) r 0 truncate
            durOfBal' :: Int
            durOfBal' = fromIntegral durOfBal
            lastInst = round $ rawCalcInstCl (fromIntegral $ capBeforeLast) 1 r 0
            capBeforeLast = calcCapAfterN c i (n - 1 + durOfBal') d r
            unfoldedBalloon | durOfBal > fromIntegral x = replicate x newI
                            | otherwise                 = replicate durOfBal' i ++ [lastInst]
                where newI = myRound (rounding param) $
                             rawCalcInstCl (fromIntegral $ calcCapBeforeBal b r) x r 0
        in lift $ newLoanR c $ replicate d 0 ++ replicate (n-1) i ++ unfoldedBalloon

    extract (UnfdBalloon p n d r _ _) = InstalmentLoanData p n d r

-- | Loan type based on Balloon Plus type. It differs from Balloon Plus that is unfolds balloon instalment so
--   that all instalments are equal, except the last one which is less or equal.
instance ClassicLoan UnfdBalloonPlus where
    newLoanI (UnfdBalloonPlus c n d rE b x) =
        ask >>= \param ->
        let   r = cE2N (freq param) rE
              i = myRound (rounding param) $
                  rawCalcInstBalPlus (fromIntegral c) n r d (fromIntegral b)
              durOfBal :: Integer
              durOfBal  = calcDurCl (fromIntegral $ calcCapBeforeBal b r)
                                    (fromIntegral  i) r 0 truncate
              durOfBal' :: Int
              durOfBal' = fromIntegral durOfBal
              lastInst = round $ rawCalcInstCl (fromIntegral $ capBeforeLast) 1 r 0
              capBeforeLast = calcCapAfterN c i (n +  durOfBal') d r
              unfoldedBalloon | durOfBal > fromIntegral x = replicate x newI
                              | otherwise                 = replicate durOfBal' i ++ [lastInst]
                  where newI = myRound (rounding param) $
                               rawCalcInstCl (fromIntegral $ calcCapBeforeBal b r) x r 0
        in lift $ newLoanR c $ replicate d 0 ++ replicate n i ++ unfoldedBalloon

    extract (UnfdBalloonPlus p n d r _ _) = InstalmentLoanData p n d r

-- | Loan type Balloon like. It differs from Balloon that its regural instalment
--   is given and balloon amount is to be calculated.
instance ClassicLoan ReversBalloon where
    newLoanI (ReversBalloon c n d rE i) =
        ask >>= \param ->
        let   r = cE2N (freq param) rE
              b = myRound (rounding param) $
                  rawCalcBalBal (fromIntegral c) n r d (fromIntegral i)
        in lift $ newLoanR c (replicate d 0 ++ replicate (n-1) i ++ [b])

    extract (ReversBalloon p n d r _) = InstalmentLoanData p n d r
    
-- | Loan type Balloon like. Its all instalment equal zero except the last one which equals principal
--   and the first one which contains all interest.
instance ClassicLoan Bullet where
    newLoanI (Bullet c n d rE) =
        ask >>= \param ->
        let   r = cE2N (freq param) rE
              fstInst = myRound (rounding param) $
                        rawCalcMaxFstInst (fromIntegral c) n r d
        in lift $ newLoanRIL (fromIntegral fstInst) c $
                  replicate d 0 ++ [fstInst] ++ replicate (n-2) 0 ++ [c]

    extract (Bullet p n d r) = InstalmentLoanData p n d r
    
-- =================== Additional instances ====================

instance Balloons Balloon where
   balloon (Balloon _ _ _ _ b) = b

instance Balloons BalloonPlus where
   balloon (BalloonPlus _ _ _ _ b) = b

instance Balloons UnfdBalloon where
   balloon (UnfdBalloon _ _ _ _ b _) = b

instance Balloons UnfdBalloonPlus where
   balloon (UnfdBalloonPlus _ _ _ _ b _) = b

instance Balloons Bullet where
   balloon (Bullet p _ _ _ ) = p

instance UnfdBalloons UnfdBalloon where
   eXtendedDuration (UnfdBalloon _ _ _ _ _ x) = x

instance UnfdBalloons UnfdBalloonPlus where
   eXtendedDuration (UnfdBalloonPlus _ _ _ _ _ x) = x

-- =======================================================================
-- =========== Functions not yet or ever used ============================
-- =======================================================================

-- | Adjusts 1st instalment of 'InstalmentPlan' by addind giving amout to
--   instalment amount and to paid late interest (the latter due to instalment balance rule).
--   There is no amount control.
adj1stInst :: Amount         -- ^ Amount of late interest 1st instalment is to be adjusted by
           -> InstalmentPlan
           -> InstalmentPlan
adj1stInst iL ip = adjIPL iL (head ip) : tail ip
          
adjIPL iL ipl = ipl { iplInst = newI } 
   where  i = iplInst ipl
          newI = i {iAmt = iAmt i  + iL
                   ,iIntPaid = iIntPaid i + iL
                   }
                   
callLateInstalment ipl = ipl { iplInst = newI } 
   where newI = i {iAmt = iAmt i - iIntPaid i
                  ,iIntPaid = 0
                  }
         i = iplInst ipl

-- | Calculates initial part of the loan - means doesn't have to finish with capital 0 at the end.
initLoan :: Amount        -- ^ capital.
         -> [Amount]      -- ^ list of instalment amounts
         -> Rate          -- ^ interest rate in frequency units
         -> Interest      -- ^ late interest on the begining. Ignored if capital == 0
         -> InstalmentPlan
initLoan _ [] _ _      = []
initLoan c (i:is) r iL = newIPL : initLoan (iplPrincipal newIPL) is r (iplIntLate newIPL)
    where newIPL = newInstalmentPlanLine c iL r i

-- | Calculates backwards last part of the loan.
tailLoan :: [Amount]      -- ^ list of instalment amounts
         -> Rate          -- ^ interest rate in frequency units
         -> InstalmentPlan
tailLoan is r = reverse $ tailLoan' 0 (reverse is) r
    where tailLoan' _ [] _     = []
          tailLoan' c (i:is) r = newIPL : tailLoan' (iRepayment $ iplInst $ newIPL) is r
              where newIPL = prevInstalmentPlanLine c r i


