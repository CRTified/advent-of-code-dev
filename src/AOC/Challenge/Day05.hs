-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day05 (
     day05a
   , day05b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Maybe (isNothing, catMaybes)
import Text.Read (readMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, space, newline, string)
import Data.Ix (inRange)

type Point = (Int, Int)
data Line = Line Point Point

type Parser = Parsec Void String

-- A point are two digits separated by ,
pointP :: Parser Point
pointP = do
  x <- read <$> some digitChar
  string ","
  y <- read <$> some digitChar
  return (x, y)

-- A line are two points separated by ->
lineP :: Parser Line
lineP = do
  p1 <- pointP
  string " -> "
  p2 <- pointP
  space
  return $ Line p1 p2

-- two vectors are colinear in 2D if their scalar product is 0
isColinear :: Point -> Point -> Bool
isColinear (x1, y1) (x2, y2) = 0 == x1 * y2 - y1 * x2

-- A point is on the line if the two endpoints of a line are colinear when
-- shifting the origin to the point under test
isOnLine :: Point -> Line -> Bool
isOnLine (xp, yp) (Line (x1, y1) (x2, y2)) =
  and [ (inRange (x1, x2) xp) || (inRange (x2, x1) xp)
      , (inRange (y1, y2) yp) || (inRange (y2, y1) yp)
      , isColinear (x1 - xp, y1 - yp) (x2 - xp, y2 - yp)
      ]

-- A line is straight if one of the coordinates of the endpoints is the same
isStraight :: Line -> Bool
isStraight (Line (x1, y1) (x2, y2)) = (x1 == x2) || (y1 == y2)

-- The bounding box around the playing field is just the limit of all
-- lines in all four directions
getBoundingBox :: [Line] -> (Int, Int, Int, Int)
getBoundingBox = foldr (update) (maxBound, maxBound, minBound, minBound)
  where
    update (Line (x1, y1) (x2, y2)) (minx, miny, maxx, maxy) =
      ( minimum [x1, x2, minx]
      , minimum [y1, y2, miny]
      , maximum [x1, x2, maxx]
      , maximum [y1, y2, maxy]
      )

-- The resulting map is just a 2D map where the multiplicity for each point is counted
drawMap :: [Line] -> [[Int]]
drawMap ls = map (map (countLines)) [[(x, y) | y<- [miny..maxy]] | x <- [minx..maxx]]
  where
    (minx, miny, maxx, maxy) = getBoundingBox ls
    countLines p = length $ filter (isOnLine p) ls

-- Filter the lines for straight ones and count how many entries in the map are >=2
day05a :: [Line] :~> Int
day05a = MkSol
    { sParse = parseMaybe (some lineP)
    , sShow  = show
    , sSolve = Just . length . (filter (>= 2)) . concat . drawMap . (filter isStraight)
    }

-- Draw the map and count how many entries in the map are >=2
day05b :: [Line] :~> Int
day05b = MkSol
    { sParse = parseMaybe (some lineP)
    , sShow  = show
    , sSolve = Just . length . (filter (>= 2)) . concat . drawMap
    }
