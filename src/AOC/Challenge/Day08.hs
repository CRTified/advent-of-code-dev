-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day08 (
     day08a
   , day08b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, hspace, space, string)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)


type Parser = Parsec Void String

type Display = Set Char
type DisplayMapping = Map Char Char

parseDisplay :: DisplayMapping -> Display -> Int
parseDisplay m d
  | mappedDisplay == "abcefg" = 0
  | mappedDisplay == "cf" = 1
  | mappedDisplay == "acdeg" = 2
  | mappedDisplay == "acdfg" = 3
  | mappedDisplay == "bcdf" = 4
  | mappedDisplay == "abdfg" = 5
  | mappedDisplay == "abdefg" = 6
  | mappedDisplay == "acf" = 7
  | mappedDisplay == "abcdefg" = 8
  | mappedDisplay == "abcdfg" = 9
  | otherwise = error $ (mappedDisplay) ++ (show d)
  where
    mappedDisplay = sort $ map (\x -> Map.findWithDefault x x m) $ Set.toList d

displayP :: Parser Display
displayP = do
  value <- some $ oneOf "abcdefg"
  hspace
  return $ Set.fromList value

lineP :: Parser ([Display], [Display])
lineP = do
  learn <- count 10 displayP
  string "|"
  hspace
  target <- count 4 displayP
  space
  return (learn, target)

inputP :: Parser [([Display], [Display])]
inputP = some lineP

findMapping :: [Display] -> DisplayMapping
findMapping ds = Map.fromList [(segA, 'a'), (segB, 'b'), (segC, 'c'),
                               (segD, 'd'), (segE, 'e'), (segF, 'f'), (segG, 'g')]
  where
    dig1 = head $ filter (((==) 2) . Set.size) ds -- 1 is the only digit with 2 segments
    dig4 = head $ filter (((==) 4) . Set.size) ds -- 4 is the only digit with 4 segments
    dig7 = head $ filter (((==) 3) . Set.size) ds -- 7 is the only digit with 3 segments
    dig8 = head $ filter (((==) 7) . Set.size) ds -- 8 is the only digit with 7 segments
    dig3 = head $ filter (\x -> 2 == (Set.size $ Set.difference x dig7)) $ -- 3 is the only digit where 3 ^ 7 has 2 segments active *and* it's not 1/4/7/8
                  filter ((flip notElem) [dig1, dig4, dig7, dig8]) ds
    dig9 = Set.union dig3 dig4 -- 9 is the union of 3 and 7
    dig2 = head $ -- 2 is problematic
             filter (segB `Set.notMember`) $ --It is not allowed to have segment B
             filter (\x -> 1 == (Set.size $ Set.difference x dig3)) $ -- its difference with 3 can only have one segment
             filter (\x -> x `notElem` [dig1, dig3, dig4, dig7, dig8, dig9]) ds -- And don't even try the numbers we already know
    segA = Set.elemAt 0 $ dig7 `Set.difference` dig1
    segB = Set.elemAt 0 $ dig4 `Set.difference` dig3
    segC = Set.elemAt 0 $ dig1 `Set.intersection` dig2
    segD = Set.elemAt 0 $ (dig9 `Set.intersection` dig3) `Set.intersection` (dig4 `Set.difference` dig1)
    segE = Set.elemAt 0 $ dig8 `Set.difference` dig9
    segF = Set.elemAt 0 $ (Set.fromList "abcdefg") `Set.difference` (Set.fromList [segA, segB, segC, segD, segE, segG]) -- The last one
    segG = Set.elemAt 0 $ (dig9 `Set.difference` dig4) `Set.difference` (Set.singleton segA)

solveLine :: ([Display], [Display]) -> [Int]
solveLine (train, test) = map (parseDisplay m) test
  where
    m = findMapping train

buildNum :: [Int] -> Int
buildNum = go 0
  where
    go :: Int -> [Int] -> Int
    go n [] = n
    go n (x:xs) = go (n * 10 + x) xs

day08a :: [([Display], [Display])] :~> Int
day08a = MkSol
    { sParse = \raw -> case parse inputP "" raw of
                         Left e -> error $ errorBundlePretty e
                         Right v -> return v
    , sShow  = show
    , sSolve = \input -> Just $ sum $ map (\r -> length $ filter ((flip elem) ([1, 4, 7, 8] :: [Int])) r) $ map (solveLine) input
    }

day08b :: [([Display], [Display])] :~> Int
day08b = MkSol
    { sParse = \raw -> case parse inputP "" raw of
                         Left e -> error $ errorBundlePretty e
                         Right v -> return v
    , sShow  = show
    , sSolve = \input -> Just $ sum $ map (buildNum . solveLine) input
    }
