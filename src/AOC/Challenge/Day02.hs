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
  
type Parser = Parsec Void String
data SubmarineCommand = SCForward Int | SCDown Int | SCUp Int

inputP :: Parser [SubmarineCommand]
inputP = some $ do
  r <- choice [ string "forward " >> (SCForward . read) <$> some digitChar
              , string "down " >> (SCDown . read) <$> some digitChar
              , string "up " >> (SCUp . read) <$> some digitChar
              ]
  space
  return r

day02a :: [SubmarineCommand] :~> Int
day02a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \input -> (\(a, b) -> Just $ a * b) $ (foldl (move) (0, 0) input)
    }
    where
      move :: (Int, Int) -> SubmarineCommand -> (Int, Int)
      move (hpos, depth) (SCForward v) = (hpos + v, depth)
      move (hpos, depth) (SCDown v) = (hpos, depth + v)
      move (hpos, depth) (SCUp v) = (hpos, depth - v)

day02b :: [SubmarineCommand] :~> Int
day02b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \input -> (\(a, b, _) -> Just $ a * b) $ (foldl (move) (0, 0, 0) input)
    }
    where
      move :: (Int, Int, Int) -> SubmarineCommand -> (Int, Int, Int)
      move (hpos, depth, aim) (SCForward v) = (hpos + v, depth + v * aim, aim)
      move (hpos, depth, aim) (SCDown v) = (hpos, depth, aim + v)
      move (hpos, depth, aim) (SCUp v) = (hpos, depth, aim - v)
