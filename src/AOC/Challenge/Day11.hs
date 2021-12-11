-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day11 (
     day11a
   , day11b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, space)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State.Lazy as ST

type Parser = Parsec Void String

type Coordinate = (Int, Int)
data Octopus = Energy Int | Flashed
type Game = Map Coordinate Octopus

rowP :: Parser [Octopus]
rowP = some $ (Energy . read) <$> count 1 digitChar

inputP :: Parser Game
inputP = do
  rows <- rowP `sepBy` space
  return $ M.fromList $ concat [ map (\(x, h) -> ((y, x), h)) rs | (y, rs) <- zip [0..] $ map (zip [0..]) rows]

getNeighbours :: Coordinate -> [Coordinate]
getNeighbours (x, y) = [ (x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]

addEnergy :: Octopus -> Octopus
addEnergy (Energy e) = Energy $ e + 1
addEnergy Flashed = Flashed

resetEnergy :: Octopus -> Octopus
resetEnergy Flashed = Energy 0
resetEnergy n = n

withEnergy :: (Int -> Bool) -> Octopus -> Bool
withEnergy _ Flashed = False
withEnergy f (Energy e) = f e

step :: ST.State Game Int
step = do
  modify $ M.map (addEnergy)
  n <- flash
  modify $ M.map (resetEnergy)
  return n

flash :: ST.State Game Int
flash = do
  toFlash <- gets $ M.filter (withEnergy $ (<) 9)
  if M.null toFlash then do
    return 0
    else do
    let neighbours = (concat $ map (getNeighbours) $ M.keys toFlash)
    modify $ M.union (M.map (const Flashed) toFlash)
    modify $ \x -> foldl (\acc k -> M.alter (fmap addEnergy) k acc) x neighbours
    newFlashes <- flash
    return $ (M.size toFlash) + newFlashes
   
day11a :: Game :~> Int
day11a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = Just . sum . (evalState (replicateM 100 step))
    }

day11b :: Game :~> Int
day11b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = Just . (\(i, _) -> i +1) . last . (takeWhile (((/=) 100) . snd)) . (zip ([1..])) . (evalState (sequence $ repeat step))
    }
