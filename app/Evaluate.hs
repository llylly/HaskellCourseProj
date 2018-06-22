module Main where

import Guess
import Tester
import System.Random
import Text.Printf
import Data.List
import Data.Monoid
import Control.Monad
import Control.Monad.List
import Control.Monad.State.Lazy
import Control.Monad.Writer

valid :: Int -> Bool
valid n = 100 <= n && n <= 9999 && noDup n where
  noDup x = length (Data.List.nub $ printf "%04d" x :: String) == 4

sample :: RandomGen g => g -> [Int] -> Int -> Int -> [Int]
sample g xs n 0 = []
sample g xs n m = let (v, g') = randomR (0, n-1) g in xs !! v: (sample g' xs n (m-1))

main :: IO ()
main = do
  g <- newStdGen
  let allNum = filter valid [100..9999]
      allNumLen = length allNum
      tryNum = [(x,y) | x <- sample g allNum allNumLen 100, y <- [1..3]]
  all <- forM tryNum (\(x,y) -> do
    g <- newStdGen
    let s = initialize valid
    (_, r) <- runWriterT (runStateT (playSilence x) (g, s))
    putStrLn $ "Num " ++ (show x) ++ " Try " ++ (show y) ++ " Times " ++ (show (getSum r))
    return (getSum r))
  putStrLn $ "AvgTimes=" ++ (show ((fromIntegral . sum) all / (fromIntegral . length) all))
  return ()
