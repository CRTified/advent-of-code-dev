{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day03 (
     day03a
   , day03b
  ) where

import           AOC.Prelude
import Data.Bits
import Control.Arrow ((&&&))

data Tree a = Leaf a | Node a (Tree a) (Tree a)
data Direction = L | R | S deriving (Eq, Show)

replicateTree :: a -> Int -> Tree a
replicateTree v 0 = Leaf v
replicateTree v n = Node v (replicateTree v (n-1)) (replicateTree v (n-1))

addToTree :: Tree Int -> String -> Tree Int
addToTree (Leaf v) _ = Leaf (v + 1)
addToTree t [] = t
addToTree (Node f l r) ('1':xs) = Node (f + 1) (l) (addToTree r xs)
addToTree (Node f l r) ('0':xs) = Node (f + 1) (addToTree l xs) (r)
addToTree (Node f l r) (_:xs) = Node (f + 1) (addToTree l xs) (r)

parseToTree :: [String] -> Tree Int
parseToTree input = foldr (flip addToTree) (replicateTree 0 len) input
  where
    len = length $ input !! 0

getField :: Tree a -> a
getField (Leaf f) = f 
getField (Node f _ _) = f

traverseTree :: ((a, a, a) -> Direction) -> Tree a -> [Direction]
traverseTree _ (Leaf _) = []
traverseTree p (Node f l r)
  | nextDir == L = L : (traverseTree p l)
  | nextDir == R = R : (traverseTree p r)
  | nextDir == S = [S]
  where
    nextDir = p (f, getField l, getField r)

dirToInt :: [Direction] -> Int
dirToInt = foldl (go) 0
  where
    go n S = n `shift` 1
    go n L = n `shift` 1
    go n R = (n `shift` 1) .|. 1
  
singleton :: a -> [a]
singleton x = [x]

levels :: Tree a -> [[a]]
levels (Leaf f) = [[f]]
levels (Node f l r) = [f] : (zipWith (++) (levels l) (levels r))

fromBoolArr :: [Bool] -> Int
fromBoolArr = foldl (go) 0
  where
    go :: Int -> Bool -> Int
    go acc True = (acc `shift` 1) .|. 1
    go acc False = (acc `shift` 1)

day03a :: Tree Int :~> Int
day03a = MkSol
    { sParse = \raw -> Just $ parseToTree $ lines raw 
    , sShow  = show 
    , sSolve =  \input -> Just $ (gammaRate input) * (epsilonRate input)
    }
  where
    gammaRate i = dirToInt $ map (\(l, r) -> if l > r then L else R) $ drop 1 $ levelSum i
    epsilonRate i = dirToInt $ map (\(l, r) -> if l > r then R else L) $ drop 1 $ levelSum i
    levelSum i = map ((sum . (every 2)) &&& (sum . (every 2) . (drop 1))) $ levels i
    every :: Int -> [a] -> [a]
    every _ [] = []
    every n (x:xs) = x : (every n $ drop (n - 1) xs)
    
day03b :: Tree Int :~> Int
day03b = MkSol
    { sParse = \raw -> Just $ parseToTree $ lines raw
    , sShow  = show
    , sSolve = \input -> Just $ (oxygenRating input) * (scrubberRating input)
    }
  where
    oxygenRating input = dirToInt $ traverseTree (\(_, l, r) -> if r >= l then R else L) input
    scrubberRating input = dirToInt $ traverseTree (minPath) input
      where
        minPath (_, 0, 0) = S
        minPath (_, 0, _) = R
        minPath (_, _, 0) = L
        minPath (_, l, r) = if r >= l then L else R
