{-# LANGUAGE TupleSections #-}

{-# LANGUAGE PartialTypeSignatures #-}

module Day20 where

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
import Data.Map (Map, (!))
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map
import Control.Monad (void)
import Data.List (transpose)
import Data.Set (Set)
import qualified Data.Set as Set

type Parser = Parsec Void String

data Tile = Tile Int [String] deriving Show

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
  mapM_ print (solve' ans)


solve' xs = open
  where sides = Map.unionsWith (+) . fmap (\(_,x) -> Map.insert x 1 Map.empty) . concat . concatMap getEdges $ xs
        ids = Map.unionsWith (<>) . fmap (\(ident,x) -> Map.insert x [ident] Map.empty) . concat . concatMap getEdges $ xs
        open = concatMap (\x -> helper (x . getEdges) xs) [head, last]
        helper f =  fmap (fmap (\(_,x) -> (ids ! x, sides ! x)) . f)
        customFilter x = (==2) . length . filter (==1) . fmap snd $ x 


getEdges (Tile ident x) = fmap (fmap (ident ,)) [canonical, reflected]
  where rotated = rotate x
        canonical = [head x, last x, head rotated, last rotated]
        reflected = fmap reverse  canonical


rotate :: [[a]] -> [[a]]
rotate = transpose . fmap reverse