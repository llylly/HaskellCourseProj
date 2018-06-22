import Guess
import Interact
import System.Random
import Text.Printf
import Data.List
import Data.Monoid
import Control.Monad.State.Lazy
import Control.Monad.Writer


valid :: Int -> Bool
valid n = 100 <= n && n <= 9999 && noDup n where
  noDup x = length (Data.List.nub $ printf "%04d" x :: String) == 4

main :: IO ()
main = do
  line <- getLine
  let goal = read line :: Int
  -- goal <- fetchGoal
  putStrLn $ "I won't tell you " ++ show goal ++ " is the number"
  g <- newStdGen
  let s = initialize valid
  print $ length s
  (_, r) <- runWriterT (runStateT (play goal) (g, s))
  putStrLn $ "Bingo!"
  putStrLn $ "it takes " ++ (show (getSum r)) ++ " guesses"
