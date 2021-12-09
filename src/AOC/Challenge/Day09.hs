-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day09 (
     day09a
   , day09b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, hspace, space, string)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import Data.List (sort)

type Parser = Parsec Void String

type Coordinate = (Int, Int)
type Height = Int

rowP :: Parser [Height]
rowP = some $ read <$> count 1 digitChar

inputP :: Parser (Map Coordinate Height)
inputP = do
  rows <- rowP `sepBy` space  
  return $ M.fromList $ concat [ map (\(x, h) -> ((y, x), h)) rs | (y, rs) <- zip [0..] $ map (zip [0..]) rows]

getAdjacentCoords :: Coordinate -> [Coordinate]
getAdjacentCoords (x, y) = [ (x - 1,     y)
                           , (x    , y + 1)
                           , (x + 1, y    )
                           , (x    , y - 1)
                           ]
                           
getAdjacentHeights :: Map Coordinate Height -> Coordinate -> [Maybe Height]
getAdjacentHeights m c = map (\c' -> M.lookup c' m) $ getAdjacentCoords c
                       
getLowPoints :: Map Coordinate Height -> Map Coordinate Height
getLowPoints m = M.filterWithKey (p) m
  where
    p coord h = all (>h) $ catMaybes $ getAdjacentHeights m coord

getBasins :: Map Coordinate Height -> [Set Coordinate]
getBasins m = map (floodFillBasin) $ M.toList $ getLowPoints m
  where
    floodFillBasin :: (Coordinate, Height) -> Set Coordinate
    floodFillBasin (_, (-1)) = S.empty
    floodFillBasin (_, 9) = S.empty
    floodFillBasin (c, h) = S.unions $ (S.singleton c) : map (recurse) (getAdjacentCoords c)
      where
        recurse :: Coordinate -> Set Coordinate
        recurse c'
          | val > h = floodFillBasin (c', val)
          | otherwise = S.empty
          where
            val = M.findWithDefault (-1) c' m
        


day09a :: Map Coordinate Height :~> Int
day09a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = Just . (M.foldr (\v acc -> v + 1 +acc) 0) . getLowPoints
    }

day09b :: Map Coordinate Height :~> Int
day09b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = Just . product . (take 3) . reverse . sort . (map length) . getBasins 
    }
