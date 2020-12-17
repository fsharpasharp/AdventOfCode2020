{-# LANGUAGE OverloadedStrings #-}

module Day17a where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Void (Void)
import Text.Megaparsec ( Parsec, optional, parseMaybe, many, some )
import Text.Megaparsec.Char
    ( char, letterChar, newline, space, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (replicateM, guard)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (elemIndices, findIndices, sort, sortBy, find, (\\))
import qualified Data.Bifunctor
import Data.Bifunctor (Bifunctor(second))

type Parser = Parsec Void String

newtype Point = Point [Int] deriving (Show, Eq, Ord) 

solve :: FilePath -> IO Int
solve fp = do
  f <- lines <$> readFile fp
  let rows = length f 
  return $ length . apply 6 newPoints . fmap (\x -> Point [x `mod` rows, x `div` rows, 0, 0]) . elemIndices '#' . concat $ f

apply :: (Eq t, Num t) => t -> (b -> b) -> b -> b
apply 0 f = id
apply n f = f . apply (n-1) f

(%) :: Point -> Point -> Bool
(%) (Point p1) (Point p2) = all (<= 1) . fmap abs $ zipWith (-) p1 p2

surroundingPoints :: Point -> [Point]
surroundingPoints op@(Point ps) = do
  let deltas = [-1,0,1]
  diffs <- replicateM (length ps) deltas
  let candidate = Point (zipWith (+) ps diffs)
  guard (candidate /= op)
  guard (candidate % op)
  return candidate

newPoints :: Foldable t => t Point -> [Point]
newPoints points = fmap fst . filter (\(p, score) -> score == 3 || p `elem` points && score == 2) $ l
  where s = concatMap surroundingPoints points
        ms = fmap (\x -> Map.insert x 1 Map.empty) s
        l = Map.toList . Map.unionsWith (+) $ ms