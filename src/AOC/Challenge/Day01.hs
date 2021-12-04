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
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, newline)

type Parser = Parsec Void String
inputP :: Parser [Int]
inputP = (read <$> some digitChar) `sepBy1` newline

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \input -> Just $ length $ filter (uncurry (<)) $ zip (input) (drop 1 input)
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \input -> Just $ length $ filter (uncurry (<)) $ zip (input) (drop 3 input)
    }
