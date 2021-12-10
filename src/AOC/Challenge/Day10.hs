{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
     day10a
   , day10b
  ) where

import AOC.Prelude
import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, space)
import Data.Stack
import Data.Maybe (catMaybes)
import Data.List (sort)

stackToList :: Stack Char -> [Char]
stackToList s
  | stackIsEmpty s = []
  | otherwise = v : (stackToList newS)
  where
    (newS, v) = case stackPop s of
                Nothing -> (stackNew, ' ')
                Just (s', v') -> (s', v')

illegalScore :: Char -> Int
illegalScore ')' = 3
illegalScore ']' = 57
illegalScore '}' = 1197
illegalScore '>' = 25137
illegalScore _ = 0

completeScore :: Char -> Int
completeScore '(' = 1
completeScore '[' = 2
completeScore '{' = 3
completeScore '<' = 4
completeScore _ = 0

expected :: Char -> Char
expected '(' = ')'
expected '[' = ']'
expected '{' = '}'
expected '<' = '>'
expected n = n

score :: String -> (Maybe Int, Maybe Int)
score = runStack stackNew
  where
    runStack acc [] = (Nothing, Just $ scoreStack acc)
    runStack acc ('(':cs) = runStack (stackPush acc '(') cs
    runStack acc ('[':cs) = runStack (stackPush acc '[') cs
    runStack acc ('{':cs) = runStack (stackPush acc '{') cs
    runStack acc ('<':cs) = runStack (stackPush acc '<') cs
    runStack acc (c:cs)
      | c == expected v = runStack nxt cs
      | otherwise = (Just $ illegalScore c, Nothing)
      where
        (nxt, v) = case stackPop acc of
                     Nothing -> (stackNew, ' ')
                     Just (s, val) -> (s, val)
    scoreStack :: Stack Char -> Int
    scoreStack s = foldl (\acc v -> acc * 5 + completeScore v) 0 $ stackToList s
    

day10a :: [String] :~> Int
day10a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . sum . catMaybes . (map (fst . score))
    }

day10b :: [String] :~> Int
day10b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . medianEntry . sort . catMaybes . (map (snd . score))
    }
  where
    medianEntry :: [a] -> a
    medianEntry xs = xs !! (((length xs) - 1) `div` 2)
  
