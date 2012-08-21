---------------------------------------------------------
--
-- Module        : FinancingConfiguration
-- Copyright     : Bartosz Wójcik (2010)
-- License       : All rights reserved
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Financing configuration.
---------------------------------------------------------
-- | Module provides predefined configurations of financing parameters.

module FinancingConfiguration
where

--import FinancingType (FstInstRule (..)
--                     ,FstIntRule (..)
--                     ,LateIntOfGracePeriodRule (..)
--                     )
import CalcCalendar
import BasicType
import Parameters


finTypeCtlm = FP Monthly
                 Monthly
                 (FstInstDays 15)
                 FstIntTill1stInst
                 Y360Specific
                 LateIntFstSignInst

finTypeUC = FP Monthly
                      Monthly
                      FstInstFullFreqAfter
                      FstIntTillNextDue
                      Y360
                      LateIntFstSignInst

finTypeUC2 = FP Monthly
                      Daily
                      FstInstFullFreqAfter
                      FstIntTillNextDue
                      Y360
                      LateIntFstSignInst

finTypeCtlmA = FP Monthly
                 Monthly
                 (FstInstDays 15)
                 FstIntTillNextDue
                 Y360Specific
                 LateIntFstSignInst

finTypeCtlmB = FP Monthly
                 Monthly
                 (FstInstDays 28)
                 FstIntTill1stInst
                 Y360Specific
                 LateIntFstSignInst

finTypeCtlmC = FP Monthly
                 Monthly
                 (FstInstDays 25)
                 FstIntTillNextDue
                 Y360Specific
                 LateIntFstSignInst

finTypeCtlmD = FP Monthly
                  Monthly
                  (FstInstDays 28)
                  FstIntTill1stInstGT0
                  Y360Specific
                  LateIntFstSignInst

finTypeCtlmE = FP Monthly
                  Monthly
                  (FstInstDays 28)
                  FstIntCtlm
                  Y360Specific
                  LateIntFstSignInst

finTypeEx1 = FP Daily
                Daily
                FstInstFullFreqAfter
                FstIntDaily
                Y360
                LateIntFstSignInst

finTypeEx2 = FP Monthly
                Monthly
                FstInstFullFreqAfter
                FstIntDaily
                Y360
                LateIntFstSignInst

finTypeEx3 = FP Yearly
                Yearly
                FstInstFullFreqAfter
                FstIntDaily
                Y360
                LateIntFstSignInst
                
finTypeEx4 = FP Monthly
                Monthly
                (FstInstDays 4)
                FstIntDaily
                Y360
                LateIntFstSignInst

paramEx1 = Param (IPP Daily Truncated) finTypeEx1
paramEx2 = Param (IPP Monthly Truncated) finTypeEx2
paramEx3 = Param (IPP Yearly Truncated) finTypeEx3
paramEx4 = Param (IPP Monthly Truncated) finTypeEx4
paramEx5 = Param (IPP Monthly Truncated) finTypeEx5

finTypeEx5 = FP Monthly
                Monthly
                FstInstFullFreqAfter
                FstIntDaily
                Y360
                LateIntFstInst
                                

