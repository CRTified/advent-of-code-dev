-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day02 (
     day02a
   , day02b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (space, hspace)

type Parser = Parsec Void String

data Move = Rock | Paper | Scissors deriving stock (Eq, Show)
data Outcome = Loss | Draw | Win deriving stock (Eq, Show)
type Strategy = (Move, Move)
type StrategyGuide = [Strategy]

type Strategy2 = (Move, Outcome)
type StrategyGuide2 = [Strategy2]

type Score = Int

moveP :: Parser Move
moveP = do
  c <- oneOf "ABC"
  return case c of
              'A' -> Rock
              'B' -> Paper
              'C' -> Scissors
              _ -> error "Invalid Parse"

responseP :: Parser Move
responseP =  do
  c <- oneOf "XYZ"
  return case c of
              'X' -> Rock
              'Y' -> Paper
              'Z' -> Scissors
              _ -> error "Invalid Parse"

outcomeP :: Parser Outcome
outcomeP =  do
  c <- oneOf "XYZ"
  return case c of
              'X' -> Loss
              'Y' -> Draw
              'Z' -> Win
              _ -> error "Invalid Parse"

strategyP :: Parser Strategy
strategyP = do
  m <- moveP
  hspace
  r <- responseP
  return (m, r)

inputP :: Parser StrategyGuide
inputP = strategyP `sepEndBy` space

strategy2P :: Parser Strategy2
strategy2P = do
  m <- moveP
  hspace
  o <- outcomeP
  return (m, o)

input2P :: Parser StrategyGuide2
input2P = strategy2P `sepEndBy` space

moveScore :: Move -> Score
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

outcomeScore :: Outcome -> Score
outcomeScore Loss = 0
outcomeScore Draw = 3
outcomeScore Win = 6

gameScore :: Strategy -> Score
gameScore (m, r) = (moveScore r) + outcomeScore outcome
  where
    outcome :: Outcome
    outcome
      | m == r = Draw
      | (m == Scissors) && (r == Paper) = Loss
      | (m == Rock) && (r == Scissors) = Loss
      | (m == Paper) && (r == Rock) = Loss
      | True = Win

gameScore2 :: Strategy2 -> Score
gameScore2 (m, o) = (moveScore response) + outcomeScore o
  where
    response :: Move
    response
      | o == Draw = m
      | (o == Loss) && (m == Scissors) = Paper
      | (o == Loss) && (m == Rock)     = Scissors
      | (o == Loss) && (m == Paper)    = Rock
      | (o ==  Win) && (m == Scissors) = Rock
      | (o ==  Win) && (m == Rock)     = Paper
      | (o ==  Win) && (m == Paper)    = Scissors
      | True = error "What?"

      
day02a :: StrategyGuide :~> Score
day02a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = Just . sum . (map gameScore)
    }

day02b :: StrategyGuide2 :~> Score
day02b = MkSol
    { sParse = parseMaybe input2P
    , sShow  = show
    , sSolve = Just . sum . (map gameScore2)
    }
