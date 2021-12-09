-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
     day02a
   , day02b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (string, space, digitChar, newline)
import Data.List (sort)

type Parser = Parsec Void String

presentP :: Parser (Int, Int, Int)
presentP = do
  l <- read <$> some digitChar
  string "x"
  w <- read <$> some digitChar
  string "x"
  h <- read <$> some digitChar
  return $ (l, w, h)

inputP :: Parser [(Int, Int, Int)]
inputP = presentP `sepBy` space

paperNeeded :: (Int, Int, Int) -> Int
paperNeeded (l, w, h) = 2*l*w + 2*w*h + 2*h*l + extra
  where
    extra = minimum [l * w, l * h, w * h]

ribbonNeeded :: (Int, Int, Int) -> Int
ribbonNeeded (l, w, h) = 2 * short1 + 2 * short2 + l * w * h
  where
    sortedSizes = sort [l, w, h]
    short1 = sortedSizes !! 0
    short2 = sortedSizes !! 1

day02a :: [(Int, Int, Int)] :~> Int
day02a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = Just . sum . (map (paperNeeded))
    }

day02b :: [(Int, Int, Int)] :~> Int
day02b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = Just . sum . (map (ribbonNeeded))
    }
