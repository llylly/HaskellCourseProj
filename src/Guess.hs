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

-- -- -- initialize guess candidates
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

mygroup :: EvaledState -> GroupedEvaledState
mygroup s = [((p, q), s') | p <- [0..4], q <- [0..p], let s' = map (\(_, r') -> r') . filter (\((p', q'), _) -> p' == p && q' == q) $ s, s' /= []]

randomRS :: RandomGen g => g -> Int -> Int -> ([Int], g)
randomRS g 0 r = ([], g)
randomRS g n r = (x: xs, gg)
                 where 
                   (x, g') = randomR (0, r) g
                   (xs, gg) = randomRS g' (n-1) r 

-- ---- --
-- Strategy: Expection Approximate by n-depth approximate

stateDS :: RandomGen g => GameState -> g -> Int -> (Float, Int)
-- stateDS CandidateList RandomGenerator Limit(default: 200) (Expectation, BestChoice)
stateDS [x] g l = (0, x)
stateDS s g l
  | slen <= l = minD . map (\n -> (1.0 + guessDS s n g l, n)) $ s
  | otherwise = let (indices, g') = randomRS g l (slen - 1) in
                minD . map (\i -> let n = s !! i in (1.0 + guessDS s n g' l, n)) $ Data.List.nub indices
    where slen = length s

guessDS :: RandomGen g => GameState -> Int -> g -> Int -> Float
-- guessDS CandidateList GuessNum RandomGenerator stateLimit Expectation
guessDS s n g l
  | slen <= 40 = sum (map (\(_, s) -> (fromIntegral . length $ s) * (fst . stateDS s g $ l)) gslist) / (fromIntegral slen)
  | otherwise  = (sum $ map (\(_, s) -> nlogn . length $ s) gslist) / (fromIntegral slen)
    where gslist = mygroup . stateEval s $ n; slen = length s; nlogn = \x -> (fromIntegral x) * (log . fromIntegral $ x)

initialize :: (Int -> Bool) -> GameState
initialize valid = filter valid [100..9999]

guess :: RandomGen g => g -> GameState -> (Int, GameState)
guess g s = (snd (stateDS s g 200), s)

refine :: (Int, GameState) -> (Int, Int) -> GameState
refine (x, s) (p, q) = filter (\y -> eval x y == (p, q)) s

