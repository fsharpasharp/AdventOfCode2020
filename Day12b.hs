module Day12b
  ( solve,
  )
where

import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (letterChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (Left, Right)
import Debug.Trace (trace)

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
  case name of
    'E' -> Direction East <$> L.decimal
    'N' -> Direction North <$> L.decimal
    'W' -> Direction West <$> L.decimal
    'S' -> Direction South <$> L.decimal
    'L' -> Left <$> L.decimal
    'R' -> Left . negate <$> L.decimal
    'F' -> Forward <$> L.decimal

update :: LocationDirection -> Instruction -> LocationDirection
update loc@(LocationDirection _ (x, y)) (Direction East val) = loc {waypoint = (x + val, y)}
update loc@(LocationDirection _ (x, y)) (Direction North val) = loc {waypoint = (x, y + val)}
update loc@(LocationDirection _ (x, y)) (Direction West val) = loc {waypoint = (x - val, y)}
update loc@(LocationDirection _ (x, y)) (Direction South val) = loc {waypoint = (x, y - val)}
update loc@(LocationDirection (x,y) (xChange, yChange)) (Forward val) = loc {position = (x+val*xChange, y+val*yChange)} 
update loc (Left val) = loc {waypoint = valRotate (waypoint loc) val}

valRotate :: (Num a, Integral t) => (a, a) -> t -> (a, a)
valRotate (x,y) 0 = (x,y)
valRotate (x,y) 90 = (-y,x)
valRotate (x,y) 180 = (-x,-y)
valRotate (x,y) 270 = (y,-x)
valRotate pos n = valRotate pos ((n+360) `mod` 360)

solve :: FilePath -> IO Int
solve file = do
  lines <- lines <$> readFile file
  let (LocationDirection (x,y) _) = solve' . fmap (fromJust . parseMaybe instructionP) $ lines
  return (abs x + abs y)

solve' :: [Instruction] -> LocationDirection
solve' = foldl update (LocationDirection (0, 0) (10,1))