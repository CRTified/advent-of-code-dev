-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day13 (
  day13a
  , day13b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (string, eol, digitChar, char, space)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow ((&&&))

type Coordinate = (Int, Int)
type Paper = Map Coordinate Int

type Parser = Parsec Void String
data Fold = FoldX Int | FoldY Int

-- A dot is x,y on a separate line
dotP :: Parser Coordinate
dotP = do
  x <- read <$> some digitChar
  char ','
  y <- read <$> some digitChar
  eol
  return (x, y)

-- Folds start with "fold along " and give the coordinate as [xy]=\d*
foldP :: Parser Fold
foldP = do
  _ <- string "fold along "
  choice [ string "x=" >> read <$> some digitChar >>= return . FoldX
         , string "y=" >> read <$> some digitChar >>= return . FoldY
         ]

-- Input is some dots, an empty line, some folds
inputP :: Parser (Paper, [Fold])
inputP = do
  dots <- some dotP
  space
  folds <- foldP `sepBy` space
  return (M.fromList $ map (\(x, y) -> ((x,y), 1)) dots, folds)


splitAlong :: Fold -> Coordinate -> Int -> Bool
splitAlong (FoldX xline) (x, _) _ = x < xline
splitAlong (FoldY yline) (_, y) _ = y < yline

-- Apply a fold by flipping the coordinates along the fold line
applyFold :: Paper -> Fold -> Paper
applyFold p f = case f of
  FoldX xl -> combine under (M.mapKeys (\(x, y) -> (2 * xl - x, y)) over)
  FoldY yl -> combine under (M.mapKeys (\(x, y) -> (x, 2 * yl - y)) over)
  where
    (under, over) = M.partitionWithKey (splitAlong f) p
    combine = M.unionWith (+)

-- Show the coordinate map by getting the bounding box and mapping each value to a pixel
showMap :: Map Coordinate a -> String
showMap m = '\n': unlines [ map (\x -> getPixel x y) [minX..maxX] | y <- [minY..maxY]]
  where
    (minX, maxX) = (minimum &&& maximum) $ map (fst) $ M.keys m 
    (minY, maxY) = (minimum &&& maximum) $ map (snd) $ M.keys m 
    getPixel :: Int -> Int -> Char
    getPixel x y
      | isMember = '█'
      | otherwise = ' '
      where
        isMember = M.member (x, y) m

day13a :: (Paper, [Fold]) :~> Int
day13a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \(paper, folds) -> Just $ length $ applyFold paper (head folds)
    }

day13b :: (Paper, [Fold]) :~> Paper
day13b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = showMap
    , sSolve = \(paper, folds) -> Just $ foldl (applyFold) paper folds
    }
