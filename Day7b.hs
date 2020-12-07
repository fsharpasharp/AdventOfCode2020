module Day7b
  ( solve,
  )
where

import Data.Void ( Void )
import Text.Megaparsec ( (<|>), Parsec, parseMaybe, some )
import Text.Megaparsec.Char ( char, letterChar, space, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))
import Data.Maybe (fromJust)


type Parser = Parsec Void String
type Adjective = String

data Bag = Bag Adjective Adjective deriving (Eq, Show)


parseLine :: Parser (Bag, [(Int, Bag)])
parseLine = do
  bagParent <- bagP
  wordP -- bags
  wordP -- contain
  bags <- space >> (string "no other bags." $> []) <|> some containedBagP
  return (bagParent, bags)
  

containedBagP :: Parser (Int, Bag)
containedBagP = do
  number <- space >> L.decimal
  bag <- bagP
  wordP -- bag(s)
  char '.' <|> char ','
  return (number, bag)

bagP :: Parser Bag
bagP = do
  a1 <- wordP
  a2 <- wordP
  return . Bag a1 $ a2

wordP :: Parser String
wordP = space >> some letterChar

solve :: FilePath -> IO Int
solve file = do
  f <- readFile file
  case traverse (parseMaybe parseLine) . lines $ f of
    Nothing -> error "Parsing error"
    Just xs -> return $ countTotal xs 1 initial - 1

initial :: Bag
initial = Bag "shiny" "gold"

countTotal :: (Eq b, Num a) => [(b, [(a, b)])] -> a -> b -> a
countTotal m multiplier x = case fromJust . lookup x $ m of
  [] -> multiplier
  xs -> multiplier + sum [countTotal m (multiplier*fst x) . snd $ x | x <- xs]