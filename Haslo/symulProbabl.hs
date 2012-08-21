module Main
where
import Test.QuickCheck
import Control.Monad
import System.Random
import Data.List
import qualified Data.Map as Map

import CalcConfigurationType
import CalcConstructors
import Parameters
import FinancingConfiguration
import ErrorHandling


randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)

test1 = do
      gen <- newStdGen
      let ns = randoms gen :: [Double]
      print $ minimum $ take 100 $ ns



r = choose ((-0.5)::Double,0.5)

fs :: (Double -> Double) -> Int -> Double -> [(Double -> Double)]
fs f n r = [g k | k <- [0 .. (n-1)]]
   where g k = \x -> x * (f r) ^ k

mySum :: (Double -> Double) -> Int -> Double -> [Double] -> Double
mySum f n r = sum . mapF (fs f n r)

f1 :: Double -> Double
f1 r = 1 + r

mapF [] _ = []
mapF _ [] = []
mapF (f:fs) (x:xs) = f x : mapF fs xs

splitAtAll :: Int -> [a] -> [[a]]
splitAtAll _ [] = []
splitAtAll n xs = pref : splitAtAll n suff
    where (pref,suff) = splitAt n xs

{-data Symul = Symul [Double] Double

instance Arbitrary Symul where
    arbitrary =
-}

test2 = do
      gen <- newStdGen
      print $ mySum f1 120 0.1 (map (0.5-) $ randoms gen :: [Double])

test3 = do
      gen <- newStdGen
      print $ mySum f1 120 0.1 [1,1..]
      
rE2N r = (1+r)**(1/12)-1

putStrSemiColon x = putStr x >> putStr ";"

main1 = do
      gen <- newStdGen
      let --n = 120
          --r = 0.1
          times = 1000
          justSum n r = mySum f1 n r [0..]
          oneTest n r xs = mySum f1 n r $ map (0.5 -) xs
          inputData n = splitAtAll n $ (randoms gen :: [Double])
--      print $ mySum f1 12 0.1 (map (0.5-) $ randoms gen :: [Double])
--      mapM_ (putStrLn . show . oneTest) $ take times inputData
          oneLine (n,r) = fromIntegral n : r : justSum n r :
                          (sort $ map (oneTest n r) $ take times $ inputData n)
          matrix = transpose $ map oneLine [(n,r) | n <- [12,24,48,96,120,240,360],
                                                    r <- map rE2N [0.01,0.05,0.1,0.15,0.2]]
      mapM_ (\xs -> mapM_ (putStrSemiColon . show) xs >> putStrLn "") matrix
--      forM_ [(n,r) | n <- [12,24,48,96,120,240,480,600], r <- map rE2N [0.01,0.05,0.1,0.15,0.2]] oneLine


--data Key = (Duration,Rate,Int)



main = do
      let lastCap ip = let IPL i lastC lastDefInt _ = last ip
                       in lastC
          testOneCase c n d rE =
              case runWithParam paramEx1 $ newLoanI Classical c n d rE of
                   Left (NotPaidDefferedInterest lastDefInt ip) -> lastDefInt
                   Left (NotAmortized lastC ip) -> lastC
                   Right ip -> lastCap ip
          showOneCase (c,n,d,rE) = do
                      putStr $ show n
                      putStr ";"
                      putStr $ show rE
                      putStr ";"
                      putStr $ show c
                      putStr ";"
                      putStr $ show d
                      putStr ";"
                      putStr $ show $ testOneCase c n d rE
                      putStrLn ";"
          showMyMap ((n,r,diff),nbr) = show diff ++ ";" ++
                                       show n ++ ";" ++
                                       show r ++ ";" ++
                                       show nbr ++ ";"
          testCases = [(c,n,d,rE) | c <- [100000,100011 .. 110000]
                                , n <- [3650]
                                ,d <- [0]
                                , rE <- [0.1]]
          insertOneCase myMap (c,n,d,rE)
              | Map.member (n,rE,rest) myMap = Map.adjust (+1) (n,rE,rest) myMap
              | otherwise                    = Map.insert (n,rE,rest) 1 myMap
              where rest = testOneCase c n d rE
          allTestCasesFolded = foldl' insertOneCase Map.empty testCases
      --mapM_ showOneCase testCases
      mapM_ (putStrLn . showMyMap) $ Map.toList allTestCasesFolded


