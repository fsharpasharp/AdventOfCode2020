{-# LANGUAGE PartialTypeSignatures #-}

module Day22 where

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
import Data.Sequence (foldrWithIndex, foldlWithIndex, ViewL((:<)), (|>), Seq((:<|)))
import Data.Maybe (fromJust)

type Parser = Parsec Void String


-- deckP :: Parser Deck
deckP :: Parser (Seq Int)
deckP = do
  string "Player "
  L.decimal
  char ':'
  newline
  Sequence.fromList <$> some (L.decimal <* void newline)

solve fp = do
  f <- readFile fp
  let [x, y] = fromJust . parseMaybe (some (deckP <* (void newline <|> eof))) $ f
  print (play x y)
  return $ foldrWithIndex (\i x y -> x*(i+1)+y) 0 (Sequence.reverse . play x $ y)

play (x :<| xs) (y :<| ys)
  | x < y = play xs (ys |> y |> x)
  | x > y = play (xs |> x |> y) ys
play empty ys = ys
play xs empty = xs