{-# LANGUAGE PartialTypeSignatures #-}

module Day24b where

import Control.Monad (void)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, try), Parsec, choice, optional, parseMaybe, parseTest, sepBy, some, (<|>))
import Text.Megaparsec.Char
  ( char,
    letterChar,
    newline,
    space,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (iterate')

type Parser = Parsec Void String

data Position = Position
  { east :: Int,
    northEast :: Int,
    southEast :: Int
  }
  deriving (Show, Ord, Eq)

instance Semigroup Position where
  Position x1 x2 x3 <> Position y1 y2 y3 = Position (x1 + y1) (x2 + y2) (x3 + y3)

instance Monoid Position where
  mempty = Position 0 0 0

positionP :: Parser Position
positionP = do
  x <-
    choice
      [ string "se",
        string "sw",
        string "nw",
        string "ne",
        string "e",
        string "w"
      ]
  return $ position x

position :: String -> Position
position "se" = mempty {southEast = 1}
position "sw" = mempty {northEast = -1}
position "nw" = mempty {southEast = -1}
position "ne" = mempty {northEast = 1}
position "e" = mempty {east = 1}
position "w" = mempty {east = -1}
position x = error "Not supported"

canonical :: Position -> Position
canonical (Position n ne se)
  | ne <= 0 && se <= 0 || ne >= 0 && se >= 0 = Position (n + se) (ne - se) 0
  | otherwise = Position (n + se) (ne - se) 0

lineP = do
  x <- mconcat <$> some positionP
  optional newline
  return $ canonical x

solve fp = do
  f <- readFile fp
  let (Just positions) = parseMaybe (some lineP <* eof) f
  let initial = Map.filter (== 1) . solve' $ positions
  return . Map.size $ iterate' step initial  !! 100 

step :: Map Position Int -> Map Position Int
step xs = oneNeighborBlack <> guaranteed
  where
    adjacent = intermediateStep xs
    guaranteed = Map.map (const 1) . Map.filter (== 2) $ adjacent
    oneNeighbor = Map.filter (== 1) $ adjacent
    oneNeighborBlack = Map.filterWithKey (\k _ -> k `Map.member` oneNeighbor) xs

intermediateStep :: Map Position Int -> Map Position Int
intermediateStep = Map.unionsWith (+) . fmap (\x -> Map.insert x 1 Map.empty) . concatMap addSurrounding . Map.keys
  where
    addSurrounding :: Position -> [Position]
    addSurrounding pos =
      canonical . (pos <>)
        <$> [ position "se",
              position "sw",
              position "nw",
              position "ne",
              position "e",
              position "w"
            ]

solve' :: [Position] -> Map Position Int
solve' = Map.unionsWith (\x y -> (x + y) `mod` 2) . fmap (\x -> Map.insert x 1 Map.empty)