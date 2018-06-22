module Main where

import Guess
import Tester
import System.Random
import Text.Printf
import Data.List
import Data.Monoid
import Control.Monad.State.Lazy
import Control.Monad.Writer


valid :: Int -> Bool
valid n = 100 <= n && n <= 9999 && noDup n where
  noDup x = length (Data.List.nub $ printf "%04d" x :: String) == 4

fetchGoal :: IO Int
fetchGoal = do
  gen <- newStdGen
  let (x, _) = randomR (100, 9999) gen
  if valid x then return x
             else fetchGoal

main :: IO ()
main = do
  goal <- fetchGoal
  putStrLn $ "I won't tell you " ++ show goal ++ " is the number"
  g <- newStdGen
  let s = initialize valid
  print $ length s
  (_, r) <- runWriterT (runStateT (play goal) (g, s))
  putStrLn $ "Bingo!"
  putStrLn $ "it takes " ++ (show (getSum r)) ++ " guesses"