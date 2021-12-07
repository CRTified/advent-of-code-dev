-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day07 (
     day07a
   , day07b
  ) where


import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar)
import Control.Arrow ((&&&))
import Data.Ix (range)

type Crab = Int

type Parser = Parsec Void String
inputP :: Parser [Crab]
inputP = (read <$> some digitChar) `sepBy1` (char ',')

listBounds :: Ord a => [a] -> (a, a)
listBounds = minimum &&& maximum

linearFuelCost :: Int -> Crab -> Int
linearFuelCost p c = abs $ c - p

incrFuelCost :: Int -> Crab -> Int
incrFuelCost p c = (n * (n+1)) `div` 2 -- Little Gauß
  where
    n = abs $ c - p

totalFuel :: (Int -> Crab -> Int) -> [Crab] -> Int -> Int
totalFuel f cs p = sum $ map (f p) cs

day07a :: [Crab] :~> Int
day07a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \input -> Just $ minimum $ map (totalFuel (linearFuelCost) input) $ range $ listBounds input
    }

day07b :: [Crab] :~> Int
day07b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \input -> Just $ minimum $ map (totalFuel (incrFuelCost) input) $ range $ listBounds input
    }

