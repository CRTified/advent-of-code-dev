-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day01 (
     day01a
   , day01b
  ) where

import AOC.Solver ((:~>)(..))

run :: Char -> Int -> Int
run ')' n = n - 1 
run '(' n = n + 1 
run  _  n = n

runUntil :: (Int -> Bool) -> String -> (Int, Int)
runUntil p input = go 0 0 input
  where
    go n i [] = (n, i) 
    go n i (x:xs)
      | p n = (n, i)
      | otherwise = go (run x n) (i+1) xs 

day01a :: String :~> Int
day01a = MkSol
    { sParse = Just . id
    , sShow  = show
    , sSolve = Just . fst . runUntil (const False)
    }

day01b :: String :~> Int
day01b = MkSol
    { sParse = Just . id
    , sShow  = show
    , sSolve = Just . snd . runUntil ((==) (-1))
    }
