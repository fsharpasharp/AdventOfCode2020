{-# LANGUAGE PartialTypeSignatures #-}

module Day24a where

import Control.Monad (void)
import Data.Void (Void)
import Data.Map.Strict ( (!), Map )
import qualified Data.Map.Strict as Map
import Text.Megaparsec (choice, MonadParsec (eof, try), Parsec, optional, parseMaybe, parseTest, sepBy, some, (<|>))
import Text.Megaparsec.Char
  ( char,
    letterChar,
    newline,
    space,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Position = Position {
  east :: Int,
  northEast :: Int,
  southEast :: Int
} deriving (Show, Ord, Eq)

instance Semigroup Position where
  Position x1 x2 x3  <> Position y1 y2 y3 = Position (x1+y1) (x2+y2) (x3+y3)

instance Monoid Position where
  mempty = Position 0 0 0


positionP :: Parser Position
positionP = do
  x <- choice [
          string "se",
          string "sw",
          string "nw",
          string "ne",
          string "e",
          string "w"
      ]
  return $ position x
  where
  position :: String -> Position
  position "se" = mempty {southEast = 1}
  position "sw" = mempty {northEast = -1}
  position "nw" = mempty {southEast = -1}
  position "ne" = mempty {northEast = 1}
  position "e" = mempty {east = 1}
  position "w" = mempty {east = -1}
  position x = error "Not supported"

lineP = do
  x <- mconcat <$> some positionP
  optional newline
  return $ simplify x
  where 
    simplify (Position n ne se)  
      | ne <= 0 && se <= 0 || ne >= 0 && se >= 0 = Position (n + se) (ne - se) 0
      | otherwise = Position (n + se) (ne - se) 0


solve fp = do
  f <- readFile fp
  let (Just positions) = parseMaybe (some lineP <* eof) f
  return . Map.size . Map.filter (==1) . solve' $ positions


solve' :: [Position] -> Map Position Integer
solve' = Map.unionsWith (\x y -> (x+y) `mod` 2) . fmap (\x -> Map.insert x 1 Map.empty)