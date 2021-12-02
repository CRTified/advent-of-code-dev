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
import Text.Read (readMaybe)

data SubmarineCommand = SCForward Int | SCDown Int | SCUp Int

parseSubmarineCommand :: String -> Maybe SubmarineCommand
parseSubmarineCommand s
  | (length splitted) == 2 = do
      value <- readMaybe $ splitted !! 1
      readCmd (splitted !! 0) value
  | otherwise = Nothing
  where
    splitted = words s
    readCmd :: String -> Int -> Maybe SubmarineCommand
    readCmd "forward" v = Just $ SCForward v
    readCmd "down" v = Just $ SCDown v
    readCmd "up" v = Just $ SCUp v
    readCmd _ _ = Nothing

day02a :: [SubmarineCommand] :~> Int
day02a = MkSol
    { sParse = \raw -> sequence $ map (parseSubmarineCommand) $ lines raw
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
    { sParse = \raw -> sequence $ map (parseSubmarineCommand) $ lines raw
    , sShow  = show
    , sSolve = \input -> (\(a, b, _) -> Just $ a * b) $ (foldl (move) (0, 0, 0) input)
    }
    where
      move :: (Int, Int, Int) -> SubmarineCommand -> (Int, Int, Int)
      move (hpos, depth, aim) (SCForward v) = (hpos + v, depth + v * aim, aim)
      move (hpos, depth, aim) (SCDown v) = (hpos, depth, aim + v)
      move (hpos, depth, aim) (SCUp v) = (hpos, depth, aim - v)
