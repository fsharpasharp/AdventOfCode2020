{-# LANGUAGE PartialTypeSignatures #-}

module Day22b where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (second))
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Map (Map, (!))
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map
import Data.Set (Set, (\\))
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
import Data.Sequence ((<|), foldrWithIndex, foldlWithIndex, ViewL((:<)), (|>), Seq(Empty, (:<|)))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Parser = Parsec Void String


deckP :: Parser (Seq Int)
deckP = do
  string "Player "
  L.decimal
  char ':'
  newline
  Sequence.fromList <$> some (L.decimal <* void newline)

solve :: FilePath -> IO Int
solve fp = do
  f <- readFile fp
  let [x, y] = fromJust . parseMaybe (some (deckP <* (void newline <|> eof))) $ f
  return $ foldrWithIndex (\i x y -> x*(i+1)+y) 0 (Sequence.reverse . snd . play Map.empty x $ y)

data Player = PlayerOne | PlayerTwo deriving Show

play :: Map (Seq Int, Seq Int) Bool -> Seq Int -> Seq Int -> (Player, Seq Int)
play map (x :<| xs) (y :<| ys)
  | Map.member (x <| xs, y <| ys) map = (PlayerOne, x <| xs)
  | x <= length xs && y <= length ys =
    case play newMap (Sequence.take x xs) (Sequence.take y ys) of
    (PlayerOne, _) -> playerOneWon
    (PlayerTwo, _) -> playerTwoWon
  | x < y = playerOneWon
  | x > y = playerTwoWon
  where playerOneWon = play newMap xs (ys |> y |> x)
        playerTwoWon = play newMap (xs |> x |> y) ys
        newMap =  Map.insert (x <| xs, y <| ys) True map
play map xs Empty = (PlayerOne, xs)
play map Empty ys = (PlayerTwo, ys)