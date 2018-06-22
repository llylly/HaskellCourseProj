module Guess (
  GameState, 
  initialize, 
  guess,
  refine,
) where

import System.Random
import Text.Printf
import qualified Data.List 

-- eval current comment
eval :: Int -> Int -> (Int, Int)
eval e x = (appeared, inPosition) where
  show04 :: Int -> String
  show04 = printf "%04d"
  de = show04 e
  dx = show04 x
  appeared = length $ filter (\d -> d `elem` dx) de
  inPosition = length $ filter (\(x,y) -> x == y) $ zip (reverse de) (reverse dx)

-- ---- --
-- Strategy: Naive Random Guess

-- guess candidates
type GameState = ([Int])

-- -- initialize guess candidates
-- initialize :: (Int -> Bool) -> GameState
-- initialize valid = filter valid [100..9999]

-- -- guess function: random pick one element from candidates
-- guess :: RandomGen g => g -> GameState -> (Int, GameState)
-- guess g s = (s !! index, s)
--   where
--     index = fst $ randomR (0, (length s) - 1) g

-- -- refine the game state
-- refine :: (Int, GameState) -> (Int, Int) -> GameState
-- refine (x, s) (p, q) = filter (\y -> eval x y == (p, q)) s


-- ---- --
-- Library functions

type EvaledState = [((Int, Int), Int)]
type GroupedEvaledState = [((Int, Int), [Int])]

minD :: [(Float, Int)] -> (Float, Int)
minD [x] = x
minD (x@(v, k):xs) = let mxs@(vs, ks) = minD xs in if v < vs then x else mxs

stateEval :: GameState -> Int -> EvaledState
stateEval xs g = map (\x -> (eval x g, x)) xs

mysort :: EvaledState -> EvaledState
mysort [] = []
mysort (((p, q), num): xs) = mysort [((p', q'), num') | ((p', q'), num') <- xs, p' < p || (p' == p && q' <= q)] ++ 
                             [((p, q), num)] ++ 
                             mysort [((p', q'), num') | ((p', q'), num') <- xs, p' > p || (p' == p && q' > q)]

mygroup :: EvaledState -> GroupedEvaledState
mygroup [] = []
mygroup [(fst, snd)] = [(fst, [snd])]
mygroup ((xp, xi):xs) = if xp == rxp then (rxp, xi:rxs): rs
                                     else (xp, [xi]): r
                        where r@((rxp, rxs):rs) = mygroup xs

randomRS :: RandomGen g => g -> Int -> Int -> ([Int], g)
randomRS g 0 r = ([], g)
randomRS g n r = (x: xs, gg)
                 where 
                   (x, g') = randomR (0, r) g
                   (xs, gg) = randomRS g' (n-1) r 

-- ---- --
-- Strategy: Expection Approximate by n-depth approximate

stateDS :: RandomGen g => GameState -> g -> Int -> Float -> (Float, Int)
-- stateDS CandidateList RandomGenerator CurrentDepth Coefficient (Expectation, BestChoice)
stateDS [] g d k = (0, -1)
stateDS [x] g d k = (0, x)
stateDS s g d k = minD . map (\x -> (1.0 + guessDS s (s !! x) gseed d k, s !! x)) $ Data.List.nub indices
  where
    slen = length s
    (indices, gseed) = randomRS g (22000 `div` slen) (slen - 1)

guessDS :: RandomGen g => GameState -> Int -> g -> Int -> Float -> Float
-- guessDS CandidateList GuessNum RandomGenerator CurrentDepth Coefficient Expectation
guessDS s n g 0 k = let slen = length s in 
                    k * (fromIntegral slen) / (fromIntegral . length . Data.List.nub $ map fst (stateEval s n))
guessDS s n g d k = let gslist = (mygroup . mysort . stateEval s) n :: GroupedEvaledState in
                    sum (map (\(a, bs) -> (fromIntegral . length $ bs) * (fst . stateDS bs g (d-1) $ k)) gslist) / (fromIntegral . length $ s)

initialize :: (Int -> Bool) -> GameState
initialize valid = filter valid [100..9999]

guess :: RandomGen g => g -> GameState -> (Int, GameState)
guess g s = (snd (stateDS s g 1 0.001), s)

refine :: (Int, GameState) -> (Int, Int) -> GameState
refine (x, s) (p, q) = filter (\y -> eval x y == (p, q)) s

