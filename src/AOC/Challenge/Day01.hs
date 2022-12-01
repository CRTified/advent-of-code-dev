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
import Data.List (sort)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, eol)

type Parser = Parsec Void String

type Calories = Integer
type ElfInventory = [Calories]

caloriesP :: Parser Calories
caloriesP = read <$> some digitChar

elfInventoryP :: Parser ElfInventory
elfInventoryP = caloriesP `sepEndBy` eol

inputP :: Parser [ElfInventory]
inputP = elfInventoryP `sepEndBy` eol

day01a :: [ElfInventory] :~> Calories
day01a = MkSol
  { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = Just . maximum . (map sum)
    }

day01b :: [ElfInventory] :~> Calories
day01b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = Just . sum . (take 3) . reverse . sort . (map sum)
    }
