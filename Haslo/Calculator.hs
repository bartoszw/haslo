---------------------------------------------------------
--
-- Module        : Calculator
-- Copyright     : Bartosz Wójcik (2010)
-- License       : BSD3
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- | Basic loan arithmetics.
---------------------------------------------------------
module Haslo.Calculator     (cE2N
                      ,cN2E
                      ,rawCalcInstCl
                      ,rawCalcInstBal
                      ,rawCalcInstBalPlus
                      ,rawCalcBalBal
                      ,rawCalcMaxFstInst
                      ,rateIrr
                      ,calcDurCl
                      ,calcCapBeforeBal
                      ,calcCapAfterN
                      ,calcCapCl
                      )
where

import Haslo.BasicType 
import Haslo.CalcCalendar (Freq,freqPerYear)
import Haslo.ErrorHandling

moduleName = "Calculator"
-- ============================
-- Set of calculation functions
-- ============================
-- | Converse yealry Effective rate to  Nominal one.
-- Nominal is in the frequency mode
cE2N :: Freq -> Rate -> Rate
cE2N fr r = (1+r)**(1/n) - 1
     where n = freqPerYear fr
     
-- | Converse Nominal rate to yearly Effective one.
-- Nominal is in the frequency mode
cN2E :: Freq -> Rate -> Rate
cN2E fr r = (1+r)^n - 1
     where n = freqPerYear fr

-- Raw installment and capital calculations for some type of loans (raw - not adjusted to Amount).
-- c-capital amount
-- i-installment amount
-- n-number of instalments
-- r-interest rate in same frequency as installment
-- d - 1st installment delay in frequency units
-- | Cacluates principal for given instalment amount, duration, interest rate and 1st instalment deferrment.
rawCalcCapCl :: Double   -- ^ Instalment amount deAmounted
             -> Duration -- ^ Loan duration
             -> Rate     -- ^ Interest rate
             -> Duration -- ^ 1st instalment deferrment
             -> Double
rawCalcCapCl i n 0 _ = i * (fromIntegral n)
rawCalcCapCl i n r d = i * (1-p^n) / (p^n * (1-p)) / p^d
                     where p = (1+r)**(1/12)

-- | Calculates raw instalment amount -- not yet rounded or truncated.
rawCalcInstCl :: Double   -- ^ Principal deAmounted
              -> Duration -- ^ Loan duration
              -> Rate     -- ^ Interest rate
              -> Duration -- ^ 1st instalment deferrment
              -> Double
rawCalcInstCl _ 0 _ _ = 0
rawCalcInstCl c n 0 _ = c / (fromIntegral n)
rawCalcInstCl c n r d = c * p^d * p^n * r / (p^n - 1)
                      where p = r+1

-- b - balloon installment amount
rawCalcCapBal b i n 0 d = i * fromIntegral (n-1) + b
rawCalcCapBal b i n r d = b/p^n + rawCalcCapCl i (n-1) r d
                        where p = r+1

-- | Cacluates instalment amount for given principal, duration, interest rate, 1st instalment deferrment and 
--   balloon amount.For @Balloon@ loan constructor.
rawCalcInstBal :: Double   -- ^ Principal deAmounted
               -> Duration -- ^ Loan duration
               -> Rate     -- ^ Interest rate
               -> Duration -- ^ 1st instalment deferrment
               -> Double   -- ^ Balloon amount deAmounted
               -> Double
rawCalcInstBal _ 0 _ _ _ = 0
rawCalcInstBal c n 0 _ b = (c - b) / (fromIntegral n - 1)
rawCalcInstBal c n r d b = (c * p^d - b/p^n) * p^m * r / (p^m - 1)
                         where p = r+1
                               m = n-1
--rawCalcInstBal c n r d b = (c * p^d - b*q^n)*(q-1) / (q^n - q)
--                         where p = r+1
--                               q = 1/p

-- | Cacluates ballon amount for given principal, duration, interest rate, 1st instalment deferrment and 
--   instalment amount.
rawCalcBalBal :: Double   -- ^ Principal deAmounted
              -> Duration -- ^ Loan duration
              -> Rate     -- ^ Interest rate
              -> Duration -- ^ 1st instalment deferrment
              -> Double   -- ^ Instalment amount deAmounted
              -> Double
rawCalcBalBal c n 0 _ i = c - i * (fromIntegral n - 1)
rawCalcBalBal c n r d i = (c * p^d - i * (q^n - q)/(q - 1)) * p^n
                        where p = r+1
                              q = 1/p

-- | Balloon Plus -- balloon amount + usual amount = last instalment amount
rawCalcCapBalPlus b i n 0 d = i * fromIntegral n + b
rawCalcCapBalPlus b i n r d = b/p^n + rawCalcCapCl i n r d
                        where p = (1+r)**(1/12)

-- | Cacluates instalment amount for given principal, duration, interest rate, 1st instalment deferrment and 
--   balloon amount. For @BalloonPlus@ loan constructor.
rawCalcInstBalPlus :: Double   -- ^ Principal deAmounted
                   -> Duration -- ^ Loan duration
                   -> Rate     -- ^ Interest rate
                   -> Duration -- ^ 1st instalment deferrment
                   -> Double   -- ^ Balloon amount deAmounted
                   -> Double
rawCalcInstBalPlus _ 0 _ _ _ = 0
rawCalcInstBalPlus c n 0 _ b = (c - b) / fromIntegral n
rawCalcInstBalPlus c n r d b = (c*p^(n+d) - b)*r / (p^n - 1)
                         where p = r+1

rawCalcBalBalPlus c n 0 _ i = c - i * fromIntegral n
rawCalcBalBalPlus c n r d i = c*p^d - i * (1 - p^n)/(1 - p)
                        where p = (1+r)**(1/12)

-- | Calculated maximum amount of first instalment: for @Bullet@ loan constructor.
rawCalcMaxFstInst :: Double   -- ^ Principal deAmounted
                  -> Duration -- ^ Instalment duration
                  -> Rate     -- ^ Nominal interest rate
                  -> Duration -- ^ 1st instalment deferrment
                  -> Double
rawCalcMaxFstInst _ _ 0 _ = 0
rawCalcMaxFstInst c n r d = c * p^d * (p^n - 1)/(p^(n-1))
                          where p = 1+r 
                          
-- | Instalment amount under when k-th instalment is greater than calculated one
--   by x.
--calcInstWithKth x k c n r d = rawCalcInstCl c r n d - x * (r+1)^(n-k-1) * r / ((r+1)^n - 1)
--calcInstBalWithKth x k c n r d b = rawCalcInstBal c n r d b - x * (r+1)^(n-k-1) * r / ((r+1)^(n-1) - 1)

-- Nominal interest rate calculation. Raphson-Newton algorithm.
-- i1-1st installment amount
-- d1-number of days between financing and 1st installment date
rateCl1stI :: (RealFloat a, Integral b) => a -> a -> a -> a -> a -> a -> a -> b -> a
rateCl1stI i i1 c n d d1 r count | abs (f / ff) < 0.000000001 = r - f / ff
                                 | count > 30 = -1                       -- error case
                                 | otherwise = rateCl1stI i i1 c n d d1 (r - f / ff) (count + 1)
                             where f = i*((r+1)**(n-1)- 1)/(r+1)**n / r + i1/(r+1) - c*(r+1)**(d + d1/30 - 1)
                                   ff = i*((n-1)*(r+1)**(n-1)*r - ((r+1)**(n-1) - 1) * (n*r+r+1))
                                      / (r+1)**(n+1) / r^2
                                      - i1/(r+1)^2
                                      - c*(d + d1/30 - 1)

-- is-installment list
-- f-sum is(i)/(r+1)^i - c*...
-- ff-sum -i*is(i)/(r+1)^(i+1) - c*...
-- Important! 'r' is here first, as hoc extrapolation of nominal interest rate. Suggested value [0.001 - 0.01].
-- If value is out of this range, result may not be calculated in foreseen 30 steps
rateIrr1stI :: [Amount]
            -> Amount
            -> Int
            -> ValidMonad Rate
rateIrr1stI is c d1 = rateIrr1stI' (map fromIntegral is) (fromIntegral c) (length is) 0 (fromIntegral d1) 0.1 0
rateIrr1stI' is c n d d1 r count | abs (f / ff) < 0.000000001 = return $ r - f / ff
                                 | count > 30                 = throwError $ OtherError $
                                                                       "rateIrr1stI c:"  ++
                                                                       show c ++
                                                                       " d1:" ++ show d1 ++
                                                                       " is:" ++ show is 
                                 | otherwise                  = rateIrr1stI' is c n d d1 (r - f / ff) (count + 1)
    where f = sum(map (uncurry (/)) (zip is [(1+r)^i|i <- [1..n]]))
            - c*(r+1)**(d + d1 / 30 - 1)
          ff = sum(map (uncurry (/)) (zip (map (uncurry (*)) (zip is [fromIntegral(-i)|i <- [1..n]])) [(1+r)^(i+1)|i <- [1..n]]))
             - c*(d + d1/30 - 1)

-- | General rate calculation.
--   Delay of 1st installment is integrated into instalment list
rateIrr :: [Amount]         -- ^ list of instalment amounts
        -> Amount           -- ^ initial principal
        -> ValidMonad Rate
rateIrr is c = case rateIrr' (map fromIntegral is) (fromIntegral c) (length is) 0.01 0 of
                  Right x -> Right x
                  Left _ -> rateIrr' (map fromIntegral is) (fromIntegral c) (length is) eps 0
    where eps = fromIntegral (sum is - c) / fromIntegral c / fromIntegral (length is) / 1000

rateIrr' is c n r count | epsilon < 1e-13 && result < 0 && eta > (-2) = return 0
                        | epsilon < 1e-13 && result < 0 =  throwError $ OtherError $
                                                                     "negative rate r=" ++ show result ++
                                                                     " is:" ++ show is
                        | epsilon < 1e-13            = return result
                        | count > 30                 = throwError $ OtherError $
                                                              "rateIrr c:" ++
                                                              show c ++
                                                              " is:" ++ show is
                        | otherwise                  = rateIrr' is c n (r - f / ff) (count + 1)
    where f = sum(map (uncurry (/)) (zip is [(1+r)^i|i <- [1..n]])) - c
          ff = sum $ map (uncurry (/))
                         (zip (map (uncurry (*))
                                   (zip is [fromIntegral(-i)|i <- [1..n]]))
                              [(1+r)^(i+1)|i <- [1..n]])
          epsilon = abs (f / ff)
          result = r - f / ff
          eta = result * c

rateCl :: Amount       -- ^ instalment amount
       -> Amount       -- ^ principal
       -> Duration     -- ^ duraiton in freq units
       -> Duration     -- ^ 1st instalment postponement in freq units
       -> ValidMonad Rate
rateCl i c n d = rateCl' (fromIntegral i) (fromIntegral c) (fromIntegral n) (fromIntegral d) 0.01 0
rateCl' i c n d r count | abs (f / ff) < 1e-13 = return $ r - f / ff
                        | count > 30           = throwError $ OtherError $
                                                              "rateCl c:" ++
                                                              show i ++
                                                              "c:" ++ show c ++
                                                              "n:" ++ show n ++
                                                              "d:" ++ show d
                        | otherwise                  = rateCl' i c n d (r - f / ff) (count + 1)
    where f  = i*((r+1)**n - 1) / (r+1)**n / r  - c * (r+1)**d
          ff = i*(n*(r+1)**n * r - ((r+1)**n - 1) * (n*r+r+1)) / (r+1)**(n+1) / r^2 - c * d


-- | Calculates duration of regural loan in the units of interest rate.
calcDurCl :: Double              -- ^ Principal deAmounted
          -> Double              -- ^ Instalment amount deAmounted
          -> Rate                -- ^ Nominal interest rate
          -> Duration            -- ^ Instalment duration
          -> (Double -> Amount)  -- ^ Rounding function (truncate or round) - taken from parameters
          -> Amount
calcDurCl _ 0 _ _ _ = 0
calcDurCl c i 0 _ roundingFun = roundingFun $ c / i
calcDurCl c i r d roundingFun = roundingFun $ log (i / (i + c * p^d * (1-p))) / log p
                              where p = 1+r 
                              
-- | Same calcualtions as for @rawCalcCapCl@ above but rounded to @Amount@.
calcCapCl :: Double   -- ^ Deintegraled instalment amount.
          -> Duration -- ^ Duration
          -> Rate     -- ^ Nominal interest rate
          -> Duration -- ^ First instalment deferrment 
          -> Amount
calcCapCl i n r = round . rawCalcCapCl i n r
calcCapBal b i n r = round . rawCalcCapBal b i n r
calcCapBalPlus b i n r = round . rawCalcCapBalPlus b i n r

-- | Calculates principal before last installment
calcCapBeforeBal :: Amount  -- ^ Balloon amount
                 -> Rate    -- ^ Nominal interest rate
                 -> Amount
calcCapBeforeBal b r = round $ fromIntegral b / (1+r)

-- | calculates principal after Nth installment
calcCapAfterN :: Amount    -- ^ Principal
              -> Amount    -- ^ Instalment amount
              -> Duration  -- ^ Loan duration
              -> Duration  -- ^ First instalment deferrment
              -> Rate      -- ^ Nominal interest rate
              -> Amount
calcCapAfterN c i n _ 0 = c - i * fromIntegral n
calcCapAfterN c i n d r = truncate $ fromIntegral c * (1+r)^(n+d) - fromIntegral i * (rXn - 1) / r
              where rXn = (1+r)^n


