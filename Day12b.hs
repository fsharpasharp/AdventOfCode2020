module Day12b
  ( solve,
  )
where

import Control.Monad (void)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec
  ( MonadParsec (eof),
    Parsec,
    many,
    parseMaybe,
    parseTest,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (letterChar, newline)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (Left, Right)

data Direction = East | North | West | South deriving (Show)

data Instruction = Direction Direction Int | Left Int | Forward Int deriving (Show)

data LocationDirection = LocationDirection
  { position :: (Int, Int),
    waypoint :: (Int, Int)
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
update loc@(LocationDirection _ (x, y)) (Direction East val) = loc {waypoint = (x + val, y)}
update loc@(LocationDirection _ (x, y)) (Direction North val) = loc {waypoint = (x, y + val)}
update loc@(LocationDirection _ (x, y)) (Direction West val) = loc {waypoint = (x - val, y)}
update loc@(LocationDirection _ (x, y)) (Direction South val) = loc {waypoint = (x, y - val)}
update loc@(LocationDirection (x, y) (xChange, yChange)) (Forward val) = loc {position = (x + val * xChange, y + val * yChange)}
update loc (Left val) = loc {waypoint = valRotate (waypoint loc) val}

valRotate :: (Num a, Integral t) => (a, a) -> t -> (a, a)
valRotate (x, y) 0 = (x, y)
valRotate (x, y) 90 = (- y, x)
valRotate (x, y) 180 = (- x, - y)
valRotate (x, y) 270 = (y, - x)
valRotate pos n = valRotate pos ((n + 360) `mod` 360)

solve :: FilePath -> IO Int
solve file = do
  lines <- lines <$> readFile file
  let (LocationDirection (x, y) _) = solve' . fmap (fromJust . parseMaybe instructionP) $ lines
  return (abs x + abs y)

solve' :: [Instruction] -> LocationDirection
solve' = foldl update (LocationDirection (0, 0) (10, 1))

lineParser :: Parser String
lineParser = do
  line <- some letterChar
  eof <|> void newline
  return line

groupParser :: Parser [String]
groupParser = do
  lines <- many lineParser
  eof <|> void newline
  return lines

main :: IO ()
main = do
  content <- readFile "input.txt"
  parseTest (many groupParser) content