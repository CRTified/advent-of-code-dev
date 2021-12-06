-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day06 (
     day06a
   , day06b
  ) where


import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar)
import Data.Map.Strict as M

type Fish = Integer
type Fishlist = M.Map Int Fish

type Parser = Parsec Void String
inputP :: Parser [Int]
inputP = (read <$> some digitChar) `sepBy1` (char ',')

dayTick :: Fishlist -> Fishlist
dayTick = M.foldrWithKey (updateFishs) M.empty
  where
    updateFishs :: Int -> Fish -> Fishlist -> Fishlist
    updateFishs 0 n acc = M.unionWith ((+)) acc (M.fromAscList [(6, n), (8, n)])
    updateFishs k n acc = M.unionWith ((+)) acc (M.fromAscList [(k - 1, n)])

buildFish :: [Int] -> Fishlist
buildFish = Prelude.foldr (addFish) M.empty
  where
    addFish :: Int -> Fishlist -> Fishlist
    addFish f acc = M.unionWith ((+)) acc (M.singleton f 1)

day06a :: Fishlist :~> Integer
day06a = MkSol
    { sParse = \raw -> case parse inputP "" raw of
        Left e -> error $ errorBundlePretty e
        Right r -> Just $ buildFish r
    , sShow  = show
    , sSolve = \input -> Just $ sum $ Prelude.map (snd) $ M.toList $ (iterate (dayTick) input) !! 80
    }

day06b :: Fishlist :~> Integer
day06b = MkSol
    { sParse = \raw -> case parse inputP "" raw of
        Left e -> error $ errorBundlePretty e
        Right r -> Just $ buildFish r
    , sShow  = show
    , sSolve = \input -> Just $ sum $ Prelude.map (snd) $ M.toList $ (iterate (dayTick) input) !! 256
    }
