module Day12a
  ( solve,
  )
where

import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (letterChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (Left, Right)

data Direction = East | North | West | South deriving (Show)

data Instruction = Direction Direction Int | Left Int | Forward Int deriving (Show)

data LocationDirection = LocationDirection
  { direction :: Direction,
    position :: (Int, Int)
  }
  deriving (Show)

type Parser = Parsec Void String

instructionP :: Parser Instruction
instructionP = do
  name <- letterChar
  let instruction = case name of
        'E' -> Direction East
        'N' -> Direction North
        'W' -> Direction West
        'S' -> Direction South
        'L' -> Left
        'R' -> Left . negate
        'F' -> Forward
  instruction <$> L.decimal

update :: LocationDirection -> Instruction -> LocationDirection
update loc@(LocationDirection _ (x, y)) (Direction East val) = loc {position = (x + val, y)}
update loc@(LocationDirection _ (x, y)) (Direction North val) = loc {position = (x, y + val)}
update loc@(LocationDirection _ (x, y)) (Direction West val) = loc {position = (x - val, y)}
update loc@(LocationDirection _ (x, y)) (Direction South val) = loc {position = (x, y - val)}
update loc@(LocationDirection facing _) (Forward val) = update loc (Direction facing val)
update loc@(LocationDirection direction _) (Left val) = loc {direction = valToFacing $ val + facingToVal direction}

valToFacing :: Int -> Direction
valToFacing 0 = East
valToFacing 90 = North
valToFacing 180 = West
valToFacing 270 = South
valToFacing n = valToFacing ((n+360) `mod` 360)

facingToVal :: Direction -> Int
facingToVal East = 0
facingToVal North = 90
facingToVal West = 180
facingToVal South = 270

solve :: FilePath -> IO Int
solve file = do
  lines <- lines <$> readFile file
  let (LocationDirection _ (x, y)) = solve' . fmap (fromJust . parseMaybe instructionP) $ lines
  return (abs x + abs y)

solve' :: [Instruction] -> LocationDirection
solve' = foldl update (LocationDirection East (0, 0))