{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day12 (
     day12a
   , day12b
  ) where

import AOC.Prelude hiding (some, many)
import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (string, lowerChar, upperChar, char, space)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State.Lazy as ST
import Data.List (partition, sort)

data Label = Start | End | SmallCave String | BigCave String deriving (Show, Eq, Ord)

type Graph = Map Label (Set Label)
mkGraph :: [(Label, Label)] -> Graph
mkGraph [] = M.empty
mkGraph ((v1, v2):vs) = M.unionsWith (S.union) [ M.singleton v1 (S.singleton v2)
                                               , M.singleton v2 (S.singleton v1)
                                               , mkGraph vs
                                               ]

removeVertex :: Graph -> Label -> Graph
removeVertex g v = M.map (S.delete v) (M.delete v g)

type Parser = Parsec Void String

labelP :: Parser Label
labelP = choice [ string "start" >> return Start
                , string "end" >> return End
                , some lowerChar >>= return . SmallCave
                , some upperChar >>= return . BigCave
                ]

rowP :: Parser (Label, Label)
rowP = do
  v1 <- labelP
  char '-'
  v2 <- labelP
  return (v1, v2)
  
inputP :: Parser Graph
inputP = do
  edges <- rowP `sepBy` space
  return $ mkGraph edges

getAdjacent :: Graph -> Label -> [Label]
getAdjacent g l = case M.lookup l g of
                    Nothing -> []
                    Just v -> filter ((/=) Start) $ S.toList v


getNextPaths :: Graph -> [Label] -> [[Label]]
getNextPaths _ [] = []
getNextPaths g vs = map (\x -> x : vs) $ validAdjacent vs
  where
    validAdjacent [] = []
    validAdjacent (v':vs') = filter (\x -> acceptablePath (x:v':vs') ) $ getAdjacent g v'
    
    acceptablePath :: [Label] -> Bool
    acceptablePath [] = True
    acceptablePath ((Start):[]) = True
    acceptablePath ((Start):_) = False
    acceptablePath ((SmallCave l):vs') = ((SmallCave l) `notElem` vs') && (acceptablePath vs')
    acceptablePath (_:vs') = acceptablePath vs'

isFinished :: [Label] -> Bool
isFinished ((End):_) = True 
isFinished _ = False

isSmallCave :: Label -> Bool
isSmallCave (SmallCave _) = True
isSmallCave _ = False

getCount :: Ord a => [a] -> Map a Int
getCount [] = M.empty
getCount (x:xs) = M.unionWith (+) (M.singleton x 1) (getCount xs)

findPaths1 :: Graph -> ST.State ([[Label]], [[Label]]) [[Label]]
findPaths1 g = do
  (finPaths, workPaths) <- get
  if 0 == length workPaths then
    return finPaths
    else do
    let wPath = head workPaths
    modify $ \(f, wp) -> (f, tail wp)
    let (fin, unfin) = partition (isFinished) $ getNextPaths g wPath
    modify $ \(f, wp) -> (f ++ fin, unfin ++ wp)
    findPaths1 g

getNextPaths2 :: Graph -> [Label] -> [[Label]]
getNextPaths2 _ [] = []
getNextPaths2 g vs = filter (isValid 0 g) $ map (\x -> x : vs) $ getAdjacent g (head vs)
  where
    isValid :: Int -> Graph -> [Label] -> Bool
    isValid n _ [] = n <= 1
    isValid n _ ((Start):[]) = n <= 1
    isValid _ _ ((Start):_) = False
    isValid n g' ((End):ls) = isValid n g' ls
    isValid n g' ((BigCave _):ls) = isValid n g' ls
    isValid n g' ((SmallCave l):ls) = and [ n' <= 1
                                         , lNum <= 2
                                         , isValid n' g' ls
                                         ]
      where
        lNum = 1 + (length $ filter (== (SmallCave l)) ls)
        n' = if lNum == 2 then n + 1 else n
        

    
findPaths2 :: Graph -> ST.State ([[Label]], [[Label]]) [[Label]]
findPaths2 g = do
  (finPaths, workPaths) <- get
  if 0 == length workPaths then
    return finPaths
    else do
    let wPath = head workPaths
    modify $ \(f, wp) -> (f, tail wp)
    let (fin, unfin) = partition (isFinished) $ getNextPaths2 g wPath
    modify $ \(f, wp) -> (f ++ fin, unfin ++ wp)
    findPaths2 g


day12a :: _ :~> _
day12a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \input -> Just . length $ evalState (findPaths1 input) ([], [[Start]])
    }

day12b :: _ :~> _
day12b =MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \input -> Just $ length $ evalState (findPaths2 input) ([], [[Start]])
    }
