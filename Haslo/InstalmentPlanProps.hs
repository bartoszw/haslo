---------------------------------------------------------
--
-- Module        : InstalmentPlanProps
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- | This module implements properities of 'InstalmentPlan'.
---------------------------------------------------------
module Haslo.InstalmentPlanProps (  instalmentPlanCheck
                             ,instalmentPlanLineCheck
                             ,instalmentPlanCheckM
                             ,instalmentPlanLineCheckM
                             ,initIPL
                             ,rateCheckTruncated
                             ,rateCheckTruncatedM
                             ,rateCheckRounded
                             ,rateCheckRoundedM
                           )
where

import Haslo.BasicType
import Haslo.CalcCalendar
import Haslo.CalcConstructors
import Haslo.ErrorHandling
import Haslo.Parameters
import Haslo.InstalmentPlan
--import CalcConfigurationType

-- | Condition each installment has to fulfil
instalmentCheck :: Instalment -> Bool
instalmentCheck i = iAmt i == iRepayment i + iIntPaid i

-- | Control function with diagnostic for humans.
instalmentCheckM :: Instalment -> ValidMonad ()
instalmentCheckM i | iAmt i == iRepayment i + iIntPaid i = return ()
                   | otherwise                           = throwError $ InstalmentDiscrepancy
                                                                        (iAmt i)
                                                                        (iRepayment i)
                                                                        (iIntPaid i)

-- | For control purposes artificial initial 'InstalmentPlanLine' has to be constructed
initIPL :: Amount -> Interest -> InstalmentPlanLine
initIPL cap iL = (IPL (I 0 0 0 0) cap iL 0)

-- | Condition, each InstalmentPlanLine fulfills before financing.
instalmentPlanLineCheck :: Amount
                        -> Interest
                        -> InstalmentPlanLine
                        -> Bool
instalmentPlanLineCheck capBefore lateIntBefore ipl =
    abs ((fromIntegral capBefore + lateIntBefore) * iplRate ipl - (iInterest . iplInst) ipl) < 1e-4 &&
    abs (lateIntBefore - (fromIntegral . iIntPaid . iplInst) ipl
                  + (iInterest . iplInst) ipl
                  - iplIntLate ipl) < 1e-2 &&
    capBefore - (iRepayment . iplInst) ipl == iplPrincipal ipl &&
    (instalmentCheck . iplInst)  ipl

-- | Control function with diagnostic for humans.
instalmentPlanLineCheckM :: InstalmentPlanLine
                         -> InstalmentPlanLine
                         -> ValidMonad InstalmentPlanLine
instalmentPlanLineCheckM ipl1 ipl2 =
   (instalmentCheckM . iplInst)  ipl2 >> checkInterestM >> checkLateInterestM >> checkCapitalAfterM
   where checkInterestM | abs (intReCalcul - intStored) < 1e-2 = return ipl2
                        | otherwise                = throwError $ IPLInterest capBefore
                                                                              lateIntBefore
                                                                              intStored
                                                                              (iplRate ipl2)
         checkLateInterestM | checkLateInterest = return ipl2
                            | otherwise         = throwError $ FollowUpInterestError errorLateInt
                                                                                     lateIntBefore
                                                                                     (iInterest $ iplInst ipl2)
                                                                                     (iIntPaid $ iplInst ipl2)
                                                                                     (iplIntLate ipl2)
         checkCapitalAfterM | checkCapitalAfter = return ipl2
                            | otherwise         = throwError $ FollowUpError errorPrincipal
                                                                             capBefore
                                                                            (iRepayment $ iplInst ipl2)
                                                                            (iplPrincipal ipl2)
         intReCalcul = ((fromIntegral capBefore + lateIntBefore) * iplRate ipl2)
         intStored = (iInterest . iplInst) ipl2
         checkLateInterest = abs (lateIntBefore - (fromIntegral . iIntPaid . iplInst) ipl2
                                                + (iInterest . iplInst) ipl2
                                                - iplIntLate ipl2) < 1e-2
         checkCapitalAfter = capBefore - (iRepayment . iplInst) ipl2 == iplPrincipal ipl2
         capBefore = iplPrincipal ipl1
         lateIntBefore = iplIntLate ipl1
         errorLateInt = "IPL discrepancy: late interest before + interest calculated -\
                        \ interest paid /= late interest after"
         errorPrincipal = "IPL discrepancy: principal before - principal matured /= principal after"

-- | Checks if all lines of installment plan fulfill their
--   validation rules.
instalmentPlanCheck :: Amount           -- ^ Initial principal
                    -> Interest         -- ^ Initial late interest (usually 0)
                    -> InstalmentPlan   -- ^ Instalment Plan to be checked
                    -> Bool
instalmentPlanCheck cap iL ip = (\(x,cf,iLf,i,r) -> x &&                  -- All rows of instlment plan are OK
                                                    (cf == 0 ||           -- Amortizes properly to 0
                                                     cf < fromIntegral (length ip) &&  r == 0) &&
                                                    abs iLf < 1e-2) $   -- There is no late interest left
                                 foldl check (True, cap, iL, I 0 0 0 0, 0) ip
    where check :: (Bool,Amount,Interest,Instalment,Rate)
                -> InstalmentPlanLine
                -> (Bool,Amount,Interest,Instalment,Rate)
          check (bool,cap,iL,_,_) ipl = (bool && instalmentPlanLineCheck cap iL ipl
                                        ,iplPrincipal ipl
                                        ,iplIntLate ipl
                                        ,iplInst ipl
                                        ,iplRate ipl)

-- | Control function with diagnostic for humans.
instalmentPlanCheckM :: Amount            -- ^ Initial principal
                     -> Interest          -- ^ Initial late interest (usually 0)
                     -> InstalmentPlan    -- ^ Instalment Plan to be checked
                     -> ValidMonad ()
instalmentPlanCheckM cap iL ip = foldM instalmentPlanLineCheckM (IPL (I 0 0 0 0) cap iL 0) ip
                                 >>= lastCheck
   where lastCheck (IPL i c iL r) | abs iL > 1e-2       = throwError $ NotPaidDefferedInterest iL ip
                                  | r > 0 && c /= 0     = throwError $ NotAmortized c ip
                                  | r == 0 && c > fromIntegral (length ip) = throwError $ NotAmortized c ip
                                  | otherwise           = return ()

-- | Checks whether instalment plan is complete, i.e. if there is no
--   remaining principal.
isInstalmentPlanComplete :: InstalmentPlan -> Bool
isInstalmentPlanComplete ip = (iplPrincipal. last) ip <= 0

--   All loans with fixed interest rate have to fulfil following:
--   If Truncated: interest rate is <= that given one.
--                 Add 1 to each instalment amount, recalculate interest rate and check that
--                 it's greater than given one.
--   If Rounded: if interest rate > given one: subtract 1 from each instalment amount, recalculate
--                                             interest rate and check that it's < given one.
--               if interest rate < given one: add 1 to each instalment amount, recalculate
--                                             interest rate and check that it's > given one.

-- | Checks recalculated interest rate against original one.
--   Works only for parameters where @Truncated@ is selected.
rateCheckTruncated :: Amount           -- ^ Initial principal
                   -> Interest         -- ^ Initial late interest (usually 0)
                   -> Rate             -- ^ Initial interest rate in nominal format.
                   -> InstalmentPlan   -- ^ Instalment Plan to be checked
                   -> ValidMonad Bool
rateCheckTruncated p iL r ip = liftM (iplRate . head) (newLoanRILPlus p iL 1 ip) >>= \r'' -> 
                               (return $ r' - r <= e1 && r'' > r)
    where r' = iplRate $ head ip

e1 = 1e-9

-- | Checks recalculated interest rate against original one.
--   Works only for parameters where @Rounded@ is selected.
rateCheckRounded :: Amount           -- ^ Initial principal
                 -> Interest         -- ^ Initial late interest (usually 0)
                 -> Rate             -- ^ Initial interest rate in nominal format.
                 -> InstalmentPlan   -- ^ Instalment Plan to be checked
                 -> ValidMonad Bool
rateCheckRounded p iL r ip | r == 0 = return True
                           | r' > r = liftM (iplRate . head) (newLoanRILPlus p iL (-1) ip) >>= \r'' ->
                                      (return $ r'' < r)
                           | r' < r = liftM (iplRate . head) (newLoanRILPlus p iL 1 ip) >>= \r'' ->
                                      (return $ r'' > r)
                           | otherwise = return True
    where r' = iplRate $ head ip

-- | Like @rateCheckTruncated@ with diagnostics for humans
rateCheckTruncatedM :: Amount           -- ^ Initial principal
                    -> Interest         -- ^ Initial late interest (usually 0)
                    -> Rate             -- ^ Initial interest rate in nominal format.
                    -> InstalmentPlan   -- ^ Instalment Plan to be checked
                    -> ValidMonad InstalmentPlan
rateCheckTruncatedM p iL r ip = liftM (iplRate . head) (newLoanRILPlus p iL 1 ip) >>= \r'' ->
                                case r' - r <= e1 && r'' > r of
                                     True -> return ip
                                     False -> throwError $ OtherError $ "Interest rate not fits." ++
                                                                        " Original: " ++ show r ++
                                                                        " Recalculated: " ++ show r' ++
                                                                        " Of next higher loan: " ++ show r''
    where r' = iplRate $ head ip

-- | Like @rateCheckRounded@ with diagnostics for humans
rateCheckRoundedM :: Amount           -- ^ Initial principal
                  -> Interest         -- ^ Initial late interest (usually 0)
                  -> Rate             -- ^ Initial interest rate in nominal format.
                  -> InstalmentPlan   -- ^ Instalment Plan to be checked
                  -> ValidMonad InstalmentPlan
rateCheckRoundedM p iL r ip | r == 0 = return ip
                            | r' > r = liftM (iplRate . head) (newLoanRILPlus p iL (-1) ip) >>= \r'' ->
                                      case r'' < r of
                                         True  -> return ip
                                         False -> throwError $ OtherError $ "Interest rate not fits." ++
                                                                        " Original: " ++ show r ++
                                                                        " Recalculated: " ++ show r' ++
                                                                        " Of next higher loan: " ++ show r''
                           | r' < r =  liftM (iplRate . head) (newLoanRILPlus p iL 1 ip) >>= \r'' ->
                                      case r'' > r of
                                         True  -> return ip
                                         False -> throwError $ OtherError $ "Interest rate not fits." ++
                                                                        " Original: " ++ show r ++
                                                                        " Recalculated: " ++ show r' ++
                                                                        " Of next higher loan: " ++ show r''
                           | otherwise = return ip
    where r' = iplRate $ head ip


-- | Adds given amount to each istalment and recalculates the InstalmentPlan.
newLoanRILPlus :: Amount
               -> Interest
               -> Amount
               -> InstalmentPlan
               -> ValidMonad InstalmentPlan
newLoanRILPlus p iL delta = newLoanRIL iL p . map (+| delta) . instList
    where a +| b | a + b < 0 = 0
                 | otherwise = a + b

