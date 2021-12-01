-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import AOC.Solver ((:~>)(..))
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)


data Change = Incr Int | Decr Int | Same

mkChange :: Int -> Int -> Change
mkChange a b
  | a > b = Decr $ a - b
  | a < b = Incr $ b - a
  | otherwise = Same

pairwiseList :: [a] -> [(a, a)]
pairwiseList [] = []
pairwiseList (_:[]) = []
pairwiseList (x1:x2:xs) = (x1, x2) : (pairwiseList (x2:xs))

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = \raw -> Just $ mapMaybe (readMaybe) $ lines raw
    , sShow  = show
    , sSolve = \input -> Just $ length [ a | Incr a <- changeList input]
    }
    where
      changeList = (map (uncurry mkChange)).pairwiseList 

slidingWindow :: Num a => Int -> [a] -> [a]
slidingWindow wsize = go
  where
    go xs
      | (length window) < wsize = []
      | otherwise = (sum window) : (go $ drop 1 xs)
      where
        window = take wsize xs

day01b :: _ :~> _
day01b = MkSol
    { sParse = \raw -> Just $ mapMaybe (readMaybe) $ lines raw
    , sShow  = show
    , sSolve = \input -> Just $ length [ a | Incr a <- changeList input]
    }
    where
      changeList = (map (uncurry mkChange)).pairwiseList.(slidingWindow 3)
