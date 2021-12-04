-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:

module AOC.Challenge.Day04 (
     day04a
   , day04b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Maybe (isNothing, catMaybes)
import Text.Read (readMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, hspace, newline)
import Control.Monad.State.Lazy as S

type Board = [[Maybe Int]]
type Game = [Int]


-- Parsing stuff -------------------------------------------

type Parser = Parsec Void String

-- A row shall be 5 Ints
rowP :: Parser [Maybe Int]
rowP = count 5 (hspace >> readMaybe <$> some digitChar)

-- A board shall be 5 rows
boardP :: Parser Board
boardP = count 5 (newline >> rowP)

-- A game sequence shall be a list of Ints, separated by comma
gameP :: Parser Game
gameP = (read <$> some digitChar) `sepBy1` (char ',')

-- The puzzle input is a game, and then a list of whitespace separated boards
inputP :: Parser (Game, [Board])
inputP = do
  game <- gameP
  boards <- some (newline >> boardP)
  return (game, boards)

-- Board Manipulation --------------------------------------
  
-- Fetches the nth row of the board
boardRow :: Int -> Board -> [Maybe Int]
boardRow n b = b !! n

-- Fetches the nth column of the Board
boardCol :: Int -> Board -> [Maybe Int]
boardCol n b = map (\r -> r !! n) b

-- A board has won if one column or one row is crossed out
hasWon :: Board -> Bool
hasWon b = any (all isNothing) $ [boardRow x b | x <- [0..4]] ++ [boardCol x b | x <- [0..4]]

-- To update a board, just keep all values and replace n with Nothing
updateBoard :: Int -> Board -> Board
updateBoard n = map (map replaceNum)
  where
    replaceNum Nothing = Nothing
    replaceNum (Just x)
      | n == x = Nothing
      | otherwise = Just x

-- Fetch all the remaining numbers
boardRemaining :: Board -> [Int]
boardRemaining = catMaybes . concat 

-- The solution is the product of the winning number and the sum of remaining numbers
getSolution1 :: Int -> Board -> Int
getSolution1 n b = n * (sum $ boardRemaining b)

-- Game processing -----------------------------------------

-- Keep consuming the game input until one board has won
runBingoUntilWin :: Game -> S.State [Board] (Maybe Int)
runBingoUntilWin [] = return Nothing
runBingoUntilWin (x:xs) = do
  modify $ map (updateBoard x)
  boards <- get
  if (hasWon) `any` boards then -- We have a winner
    return $ Just $ getSolution1 x $ head $ filter (hasWon) boards
    else
    runBingoUntilWin xs

-- Keep consuming the game input until the last board has won
runBingoUntilOneUnfinished :: Game -> S.State [Board] (Maybe Int)
runBingoUntilOneUnfinished [] = return Nothing
runBingoUntilOneUnfinished (x:xs) = do
  -- We do not need to keep boards that already won
  modify $ (filter (not . hasWon)) . map (updateBoard x)
  boards <- get
  if (length boards) == 1 then
    runBingoUntilWin xs -- Only one board left, keep going until it wins
    else
    runBingoUntilOneUnfinished xs


-- AoC Boilerplate -----------------------------------------
    
day04a :: (Game, [Board]) :~> Int
day04a = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \(game, boards) -> evalState (runBingoUntilWin game) boards
    }
    
day04b :: (Game, [Board]) :~> Int
day04b = MkSol
    { sParse = parseMaybe inputP
    , sShow  = show
    , sSolve = \(game, boards) -> evalState (runBingoUntilOneUnfinished game) boards
    }
