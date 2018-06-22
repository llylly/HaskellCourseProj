module Tester (
  play,
  playSilence
) where

import Guess
import System.Random
import Data.List
import Data.Monoid
import Text.Printf
import Control.Monad.State.Lazy
import Control.Monad.Writer

type Game = StateT (StdGen, GameState) (WriterT (Sum Int) IO)
  -- StateT (StdGen, GameState) a   -- maintain the change of states
  -- WriterT (Sum Int) a            -- count the nunber of guess
  -- IO a                           -- output

eval :: Int -> Int -> (Int, Int)
eval e x = (appeared, inPosition) where
  show04 :: Int -> String
  show04 = printf "%04d"
  de = show04 e
  dx = show04 x
  appeared = length $ filter (\d -> d `elem` dx) de
  inPosition = length $ filter (\(x,y) -> x == y) $ zip (reverse de) (reverse dx)


play :: Int -> Game ()
play goal = do
  (g, s) <- get
  let (g1, g2) = split g
      (x, s') = guess g1 s
      (r1, r2) = eval goal x
      s'' = refine (x,s') (r1, r2)
  put (g2, s'')
  tell (Sum 1)
  liftIO $ putStrLn $ "you guess " ++ (show x) ++ " -> (" ++ (show r1) ++ ", " ++ (show r2) ++ ")"
  when (x /= goal) $ play goal

playSilence :: Int -> Game ()
playSilence goal = do
  (g, s) <- get
  let (g1, g2) = split g
      (x, s') = guess g1 s
      (r1, r2) = eval goal x
      s'' = refine (x,s') (r1, r2)
  put (g2, s'')
  tell (Sum 1)
  -- liftIO $ putStrLn $ "you guess " ++ (show x) ++ " -> (" ++ (show r1) ++ ", " ++ (show r2) ++ ")"
  when (x /= goal) $ playSilence goal
