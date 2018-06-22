module Guess (
  GameState, 
  initialize, 
  guess,
  refine,
) where

import System.Random
import Text.Printf
import qualified Data.List 

-- work of Linyi Li
-- 2014011361

-- Eval current Reply
-- The function calculates the number of appeared digits and number of right-in-position digits
-- Inherited from framework 
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
-- In naive random guess strategy, we maintain current possible answer candidates.
-- In each round, we randomly pick one candidate out as the guess, and use response to filter out impossibles.
-- Work as my baseline.

-- guess candidates
type GameState = ([Int])

-- -- initialize guess candidates
-- -- Use valid function to initialize candidates
-- initialize :: (Int -> Bool) -> GameState
-- initialize valid = filter valid [100..9999]

-- -- guess function: random pick one element from candidates
-- guess :: RandomGen g => g -> GameState -> (Int, GameState)
-- guess g s = (s !! index, s)
--   where index = fst $ randomR (0, (length s) - 1) g

-- -- refine the game state
-- refine :: (Int, GameState) -> (Int, Int) -> GameState
-- refine (x, s) (p, q) = filter (\y -> eval x y == (p, q)) s


-- ---- --
-- Library functions
-- Ancilary functions and definitions for my strategy.

-- [(Response, QueryNumber)], Response = (p, q), where p is # of appeared digits and q is # of in-position digits
type EvaledState = [((Int, Int), Int)]
-- [(Response, QueryNumbers)], means all these numbers have the same Response, they are grouped together
type GroupedEvaledState = [((Int, Int), [Int])]

-- minD function
-- [(QueryTimeExpectation, QueryCandidate)] as input, minimum QueryTimeExpection item found and returned along with corresponding QueryCandidate.
minD :: [(Float, Int)] -> (Float, Int)
minD [x] = x
minD (x@(v, k):xs) = let mxs@(vs, ks) = minD xs in if v < vs then x else mxs

-- stateEval function
-- Calculate response for current query g for each candidate in xs
stateEval :: GameState -> Int -> EvaledState
stateEval xs g = map (\x -> (eval x g, x)) xs

-- mygroup function
-- group query numbers by their response
mygroup :: EvaledState -> GroupedEvaledState
mygroup s = [((p, q), s') | p <- [0..4], q <- [0..p], let s' = map (\(_, r') -> r') . filter (\((p', q'), _) -> p' == p && q' == q) $ s, s' /= []]

-- randomRS function
-- Generate n numbers within range [0, r], with RandomGen g as input and (RandomNumberList, RandomGen) as output.
randomRS :: RandomGen g => g -> Int -> Int -> ([Int], g)
randomRS g 0 r = ([], g)
randomRS g n r = (x: xs, gg)
                 where 
                   (x, g') = randomR (0, r) g
                   (xs, gg) = randomRS g' (n-1) r 

-- ---- --
-- Strategy: Approximate Expectation Method

stateDS :: RandomGen g => GameState -> g -> Int -> (Float, Int)
-- stateDS: s - CandidateList, g - RandomGenerator, l - Limit, return - (Expectation, BestChoice)
-- Calculate expection approximation for current state, returns expection query time and best choice of query which reaches this value.
-- Limit l is set on size of CancidateList, when size exceeds l we do sampling in the list for l times to get the best choice.
-- When size is in l, we enumerate all members in the list.
stateDS [x] g l = (0, x)
stateDS s g l
  | slen <= l = minD . map (\n -> (1.0 + guessDS s n g l, n)) $ s
  | otherwise = let (indices, g') = randomRS g l (slen - 1) in
                minD . map (\i -> let n = s !! i in (1.0 + guessDS s n g' l, n)) $ Data.List.nub indices
    where slen = length s

guessDS :: RandomGen g => GameState -> Int -> g -> Int -> Float
-- guessDS: s - CandidateList, n - GuessNum, g - RandomGenerator, l - Limit l passed to stateDS when calling it, return - Expectation
-- Calculate expection approximation when guess n in current state, returns expectation query time.
-- When candidate list size is less than 40, we brute force it to calculate exact expectation, otherwise we use log function as approximation, which is explained in the report.
guessDS s n g l
  | slen <= 40 = sum (map (\(_, s) -> (fromIntegral . length $ s) * (fst . stateDS s g $ l)) gslist) / (fromIntegral slen)
  | otherwise  = (sum $ map (\(_, s) -> nlogn . length $ s) gslist) / (fromIntegral slen)
    where gslist = mygroup . stateEval s $ n; slen = length s; nlogn = \x -> (fromIntegral x) * (log . fromIntegral $ x)

-- Required interface, intialize the candidate list.
initialize :: (Int -> Bool) -> GameState
initialize valid = filter valid [100..9999]

-- Required interface, call stateDS for handling, with limit = 200.
guess :: RandomGen g => g -> GameState -> (Int, GameState)
guess g s = (snd (stateDS s g 200), s)

-- Required interface, utilize response to refine candidate list.
refine :: (Int, GameState) -> (Int, Int) -> GameState
refine (x, s) (p, q) = filter (\y -> eval x y == (p, q)) s

