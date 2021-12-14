-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day14 (
     day14a
   , day14b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (string, alphaNumChar, space)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow ((&&&))

type Parser = Parsec Void String

type Ruleset = Map String String

templateP :: Parser String
templateP = some alphaNumChar

ruleP :: Parser (String, String)
ruleP = do
  l <- alphaNumChar
  r <- alphaNumChar
  _ <- string " -> "
  c <- alphaNumChar
  return (l:r:[], l:c:r:[])

inputP :: Parser (String, Ruleset)
inputP = do
  template <- templateP
  space
  rules <- ruleP `sepBy` space
  return (template, M.fromList rules)

findMinMax :: Char -> Map String Int -> Int
findMinMax fstChr s = maxOcc - minOcc
  where
    charCount = M.foldrWithKey (splitChars) (M.singleton fstChr 1) s
    splitChars (_:_:[]) 0 acc = acc
    splitChars (_:x2:[]) v acc = M.unionWith (+) acc (M.fromList [(x2, v)])
    splitChars _ _ acc = acc
    (minOcc, maxOcc) = (minimum &&& maximum) $ M.elems charCount

applyRuleC :: Ruleset -> Map String Int -> Map String Int
applyRuleC s m = M.foldrWithKey (updateNums) m m
  where
    updateNums :: String -> Int -> Map String Int -> Map String Int
    updateNums _ 0 acc = acc
    updateNums pair n acc = M.unionWith (+) (acc) (nextNums n pair)
    nextNums n pair = case M.lookup pair s of
                      Nothing -> M.empty
                      Just (l:c:r:[]) -> M.unionsWith (+) $ map (uncurry M.singleton) [ (l:r:[], -n), (l:c:[], n), (c:r:[], n)]
                      Just _ -> M.empty

buildInitial :: String -> Map String Int
buildInitial [] = M.empty
buildInitial (_:[]) = M.empty
buildInitial (x1:x2:xs) = M.unionWith (+) (M.singleton (x1:x2:[]) 1) $ buildInitial (x2:xs)
                                                               
day14a :: (String, Ruleset) :~> Int
day14a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \(template, rs) -> Just $ findMinMax (template !! 0) $ (iterate (applyRuleC rs) $ buildInitial template) !! 10
    }

day14b :: (String, Ruleset) :~> Int
day14b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \(template, rs) -> Just $ findMinMax (template !! 0) $ (iterate (applyRuleC rs) $ buildInitial template) !! 40
    }
