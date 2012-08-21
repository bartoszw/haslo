---------------------------------------------------------
--
-- Module        : HasloQCTestL
-- Copyright     : Bartosz Wójcik (2011)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Part of haslo. Data types used for Quick Check.
-- Properties set up.
---------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Haslo.HasloQCTestL
where

import Haslo
import Data.List
import Data.Time
import Test.QuickCheck
import Text.PrettyShow

caps = choose (1000::Amount,100000000)
caps' b = choose (b::Amount,100000000)


class Show a => ShowShort a where
    showShort :: a -> String

data GUIClassic = CL1 Classical
                | CL2 Balloon
                | CL3 BalloonPlus
                | CL4 ReversBalloon
                | CL5 Bullet
                | CL6 UnfdBalloon
                | CL7 UnfdBalloonPlus
     deriving (Eq, Ord)

instance Show GUIClassic where
   show (CL1 l) = show l
   show (CL2 l) = show l
   show (CL3 l) = show l
   show (CL4 l) = show l
   show (CL5 l) = show l
   show (CL6 l) = show l
   show (CL7 l) = show l



instance PrettyShow GUIClassic where
   showWithLen n = showWithLen n . show

instance ShowShort GUIClassic where
    showShort = filter (/= '(') . head . drop 1 . words . show

instance Enum GUIClassic where
   toEnum 0 = CL1 $ Classical 0 0 0 0
   toEnum 1 = CL2 $ Balloon 0 0 0 0 0
   toEnum 2 = CL3 $ BalloonPlus 0 0 0 0 0
   toEnum 3 = CL4 $ ReversBalloon 0 0 0 0 0
   toEnum 4 = CL5 $ Bullet 0 0 0 0
   toEnum 5 = CL6 $ UnfdBalloon 0 0 0 0 0 0
   toEnum 6 = CL7 $ UnfdBalloonPlus 0 0 0 0 0 0
--   toEnum x = toEnum $ x `mod` 7

   fromEnum (CL1 _) = 0
   fromEnum (CL2 _) = 1
   fromEnum (CL3 _) = 2
   fromEnum (CL4 _) = 3
   fromEnum (CL5 _) = 4
   fromEnum (CL6 _) = 5
   fromEnum (CL7 _) = 6

instance ClassicLoan GUIClassic where
    newLoanI (CL1 x) = newLoanI x
    newLoanI (CL2 x) = newLoanI x
    newLoanI (CL3 x) = newLoanI x
    newLoanI (CL4 x) = newLoanI x
    newLoanI (CL5 x) = newLoanI x
    newLoanI (CL6 x) = newLoanI x
    newLoanI (CL7 x) = newLoanI x

    extract (CL1 x) = extract x
    extract (CL2 x) = extract x
    extract (CL3 x) = extract x
    extract (CL4 x) = extract x
    extract (CL5 x) = extract x
    extract (CL6 x) = extract x
    extract (CL7 x) = extract x

instance Balloons GUIClassic where
    balloon (CL2 x) = balloon x
    balloon (CL3 x) = balloon x
    balloon (CL5 x) = balloon x
    balloon (CL6 x) = balloon x
    balloon (CL7 x) = balloon x
    balloon l = error $ show l ++ " is not a balloon"

instance UnfdBalloons GUIClassic where
    eXtendedDuration (CL6 x) = eXtendedDuration x
    eXtendedDuration (CL7 x) = eXtendedDuration x
    eXtendedDuration l = error $ show l ++ " is not a UnfdBalloon"

{-
data GUIFee = Fee1 FeeFinanced
            | Fee2 FeeAsLateInterest
            | Fee3 FeeLimitingFstInstalment
     deriving (Eq, Ord, Show)

instance Enum GUIFee where
   toEnum 0 = Fee1 FeeFinanced
   toEnum 1 = Fee2 FeeAsLateInterest
   toEnum 2 = Fee3 FeeLimitingFstInstalment

   fromEnum (Fee1 _) = 0
   fromEnum (Fee2 _) = 1
   fromEnum (Fee3 _) = 2

instance FeeClass GUIFee where
    addFee (Fee1 _) = addFee FeeFinanced
    addFee (Fee2 _) = addFee FeeAsLateInterest
    addFee (Fee3 _) = addFee FeeLimitingFstInstalment

instance ShowShort GUIFee where
    showShort = head . drop 1 . words . show


feeList = take 3 $ map showShort [Fee1 FeeFinanced ..]
-}

-- | Poperties of instalment loan for given input parameters.
instance Arbitrary GUIClassic where
    arbitrary = do
      p <- frequency  [--(15,return paramDT),
                      (70,return paramMT),
                      (30,return paramYT)]
      -- Duration limited to 30 years
      n <- choose (3::Duration, 30 * (freqPerYear $ freq p))
      
      -- Deferment limited to 12 instalments
      d <- choose (0::Duration,12)
      
      -- Interest rate limited to 20%. Selected like for humans: max 4 digits after decimal point.
      r <- frequency [(1,return 0)
                     ,(99,liftM ((/10^6) . fromIntegral) $ choose (0::Int,2*10^5))
                     ]
                     
      -- Balloon amount and capital limited to 10^8 (in € this makes 10^6)
      b <- caps
      c <- caps
      cBal <- caps' b

      -- Instalment for reversal balloon limited to 10^6 (in € this makes 10^4)
      i <- choose (0::Amount,100000)
      
      -- Capital of reversal balloon selected so, that balloon amount will be positive
      cRevBal <- choose (calcCapCl (fromIntegral i) n (cE2N (freq p) r) d
                        ,i * fromIntegral n * 2 + 1000000)
                        
      -- Max duration of extended period of UnfdBalloons limited so that total duration doesn't extend 30 years
      x <- choose (1::Duration, 30 * (freqPerYear $ freq p) - n + 1)

      oneof [return $ CL1 $ Classical c n d r
            ,return $ CL2 $ Balloon cBal n d r b
            ,return $ CL3 $ BalloonPlus cBal n d r b
            ,return $ CL4 $ ReversBalloon cRevBal n d r i
            ,return $ CL5 $ Bullet c n 0 r
            ,return $ CL6 $ UnfdBalloon cBal n d r b x
            ,return $ CL7 $ UnfdBalloonPlus cBal n d r b x
            ]

data TestL = TestL (GUIClassic,InstalmentPlanParam)

instance Show TestL where
   show (TestL (loan, IPP fr rd)) = show loan ++ " " ++ showWithLen 8 fr ++ showWithLen 9 rd

instance Arbitrary TestL where
    arbitrary = do
      l <- arbitrary
      p <- if duration (extract l) <= 30
              then frequency  [--(15,return paramDT),
                              (10,return paramMR),
                              (40,return paramYR),
                              (10,return paramMT),
                              (40,return paramYT)]
              else oneof [return paramMT
                         ,return paramMR
                         ]
      return $ TestL (l, p)


instance Testable (ValidMonad Bool) where
   property (Right b)  = property b
   property (Left err) = property False

instance Testable (ValidMonad ()) where
   property (Right _)  = property True
   property (Left err) = error $ show err


testLoan :: TestL -> ValidMonad InstalmentPlan
testLoan (TestL (loan, ipp)) =  runWithIPP ipp $ newLoanI loan


propInstPlan :: TestL -> ValidMonad Bool
propInstPlan p@(TestL (loan, ipp)) = testLoan p >>= \ip ->
                                     return (instalmentPlanCheck (principal $ extract loan) (fstInst p) ip)

--propInstPlan :: TestL -> ValidMonad Bool
--propInstPlan p@(TestL (_,_,amt,_,_,_)) = testLoan p >>= \ip ->
--                                         return (instalmentPlanCheck amt (fstInst p) ip)

--propInstPlanM :: TestL -> ValidMonad ()
--propInstPlanM p@(TestL (_,_,amt,_,_,_)) = testLoan p >>=
--                                          instalmentPlanCheckM amt (fstInst p)

fstInst :: TestL -> Double
fstInst (TestL (CL5 (Bullet amt dur def rat), ipp)) =
        fromIntegral $ myRound (rounding ipp) $ rawCalcMaxFstInst (fromIntegral amt) dur (cE2N (freq ipp) rat) def
fstInst _ = 0

instance Arbitrary Freq where
   arbitrary = oneof [return Daily, return Monthly, return Yearly]

-- | Properity of interest rate conversion.
--   Interest rate has to be positive.
--   Daily interest rate not greater than 100%.
prop_cEN :: (Freq,Double) -> Property
prop_cEN (f,x) = x >= 0 && (x <= 1 || f /= Daily) ==> abs (cE2N f (cN2E f x) - x) <= x * 1e-12


-- | Interest rate property doesn't work for Bullet and UnfdBalloon loans
propRate :: TestL -> ValidMonad Bool
propRate p@(TestL (loan, ipp@(IPP _ Truncated))) | fromEnum loan `elem` [0,1,2,3] =
    testLoan p >>= rateCheckTruncated (principal $ extract loan) (fstInst p) r
                                                 | otherwise = return True
    where r = cE2N (freq ipp) (rate $ extract loan)
propRate p@(TestL (loan, ipp@(IPP _ Rounded))) | fromEnum loan `elem` [0,1,2,3] =
    testLoan p >>= rateCheckRounded (principal $ extract loan) (fstInst p) r
                                                 | otherwise = return True
    where r = cE2N (freq ipp) (rate $ extract loan)

