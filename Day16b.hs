{-# LANGUAGE OverloadedStrings #-}

module Day16b where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Void (Void)
import Text.Megaparsec ( Parsec, optional, parseMaybe, many, some )
import Text.Megaparsec.Char
    ( char, letterChar, newline, space, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (guard)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (sort, sortBy, find, (\\))
import qualified Data.Bifunctor
import Data.Bifunctor (Bifunctor(second))

type Parser = Parsec Void String

data Range = Range String ((Int, Int), (Int, Int)) deriving Show

rangeP :: Parser Range
rangeP = do
  s1 <- some letterChar
  optional space
  s2 <- optional (many letterChar)
  string ": "

  lower <- L.decimal
  char '-'
  upper <- L.decimal
  let range1 = (lower, upper)

  string " or "

  lower <- L.decimal
  char '-'
  upper <- L.decimal
  let range2 = (lower, upper)
  newline

  return $ Range (s1 ++ fromMaybe "" s2) (range1, range2)


ticketP :: Parser [Int]
ticketP = do
  nums <- some (L.decimal <* (optional . char $  ','))
  newline
  return nums


valid :: Map String [Int]
valid = Map.empty

solve fp = do
  f <- readFile fp
  let Just (range, tickets) = parseMaybe p  f
  let validTickets = filterTickets range tickets

  mapM_ print . search . fmap (second ([0 .. 19] \\)). Map.toList . Map.unionsWith (++) . concatMap (concatMap (populateMap range valid) . zip [0..]) $ validTickets


search = scanl (\(x,y,z,d) (a,b,c) -> (a,b,c\\d,d++(c\\d))) (0,[],[],[]) . sort . fmap (\(x,y) -> (length y, x, y))


populateMap range valid (i,x)
  = (\(Range s  _) -> Map.insert s [i] valid) <$> filter (not . withinRange x) range

withinRange :: Int -> Range -> Bool
withinRange x (Range _ p) = withinRange' (fst p) || withinRange' (snd p)
  where withinRange' a = fst a <= x && x <= snd a


filterTickets :: (Foldable t1, Foldable t2) => t2 Range -> [t1 Int] -> [t1 Int]
filterTickets _ [] = []
filterTickets range (x:xs)
  | all valueIsValid x = x:filterTickets range xs
  | otherwise = filterTickets range xs
  where valueIsValid e = any (withinRange e) range

p :: Parser ([Range], [[Int]])
p = do
    a <- some rangeP
    newline
    string "your ticket:"
    newline
    b <- ticketP
    newline
    string "nearby tickets:"
    newline
    c <- some ticketP
    return (a,b:c)
