{-# LANGUAGE PartialTypeSignatures #-}

module Day20 where

import Control.Monad (void)
import Control.Monad.State
import Data.List (elemIndices, intercalate, intersperse, find, nub, transpose)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Sequence as Sequence
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, try), Parsec, optional, parseMaybe, parseTest, sepBy, some, (<|>))
import Text.Megaparsec.Char
  ( char,
    letterChar,
    newline,
    space,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Tile = Tile Int [String] deriving (Ord, Eq)

instance Show Tile where
  show (Tile identity _) = show identity

data Puzzle = Puzzle Tile [Maybe Puzzle] deriving (Show)

tileP :: Parser Tile
tileP = do
  string "Tile "
  id <- L.decimal
  char ':'
  newline
  tile <- some (some (char '.' <|> char '#') <* newline)
  newline
  optional newline
  return $ Tile id tile

solve :: FilePath -> _
solve fp = do
  f <- readFile fp
  let (Just ans) = parseMaybe (some tileP <* eof) f
  return $ solve' ans

align :: Maybe Puzzle -> _
align Nothing = Nothing
align (Just (Puzzle t ps)) = Just (Puzzle t (fmap (align . go) (zip ps borders)))
  where
    borders = zip [x `mod` 4 | x <- [2 ..]] (edges t)
    go (Nothing, _) = Nothing
    go (p@(Just (Puzzle t ps)), second@(i, border))
      | edges t !! i == border = p
      | edges (reflectTile t) !! i == border = reflected
      | otherwise = go (rotated, second)
      where
        reflected = Just (Puzzle (reflectTile t) (swapList ps))
        rotated = Just (Puzzle (rotateTile t) (rotateList ps))
        rotateTile (Tile i x) = Tile i (rotate x)
        reflectTile (Tile i x) = Tile i (reflectX x)
        swapList [a, b, c, d] = [c, b, a, d]
        rotateList xs = last xs : take (length xs - 1) xs

walk :: Maybe Puzzle -> Map (Int, Int) Tile
walk p = walk' p 3 10 -- Offset. TODO: Find offset programmatically.
  where
    walk' :: Maybe Puzzle -> Int -> Int -> Map (Int, Int) Tile
    walk' Nothing y x = Map.empty
    walk' (Just (Puzzle t ps)) y x = Map.singleton (y, x) t <> foldMap (\(p, (y, x)) -> walk' p y x) (zip ps coords)
      where
        coords = [(y, x + 1), (y + 1, x), (y, x -1), (y -1, x)]

addTiles :: [String] -> Tile -> _
addTiles xs (Tile _ ys) = fmap (uncurry (++)) . zip xs $ ys

monsters = fmap (elemIndices upper)
  where 
  upper = "...................#"
  middd = "#....##....##....###"
  lower = "..#..#..#..#..#..#.."

solve' :: [Tile] -> _
solve' xs = monsters . fmap (foldl addTiles (replicate 12 "")) . chunks 12 . fmap snd . Map.toList . walk . align $ tileConnections
  where
    chunks :: Int -> [a] -> [[a]]
    chunks n [] = []
    chunks n xs = take n xs : chunks n (drop n xs)
    edgeToTileMap = Map.unionsWith (++) . fmap edgeToTile $ xs
    tileConnections = evalState (go (Just . head $ xs)) Set.empty
      where
        go :: Maybe Tile -> State (Set Tile) _
        go Nothing = return Nothing
        go (Just t) = do
          seen <- get
          if t `Set.member` seen
            then return Nothing
            else do
              modify (Set.insert t)
              seen <- get
              a <- traverse (go . find (not . (`Set.member` seen)) . (edgeToTileMap !)) $ edges t
              return $ Just (Puzzle t a)

edgeToTile :: Tile -> Map [Char] [Tile]
edgeToTile t = foldMap (`Map.singleton` [t]) $ concat ([edges, reflectX . edges] <*> [t])

edges :: Tile -> [[Char]]
edges t@(Tile _ x) = [head . rotate, head, last . rotate, last] <*> [x]

rotate :: [[a]] -> [[a]]
rotate = transpose . fmap reverse

reflectX :: [[a]] -> [[a]]
reflectX = fmap reverse
