module Day19 where

import qualified Control.Applicative as Applicative
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    optional,
    parseMaybe,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char, letterChar, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as ReadP

type Parser = Parsec Void String

data Rule = Char Char | Dep [Int] | Or Rule Rule deriving (Show)

inputP :: Parser ([(Int, Rule)], [String])
inputP = do
  rules <- some ruleP
  newline
  words <- some wordP
  return (rules, words)

ruleP :: Parser (Int, Rule)
ruleP = do
  id <- L.decimal
  string ": "
  rule <- parserP <|> try orP <|> depP
  newline
  return (id, rule)

wordP :: Parser String
wordP = do
  word <- some letterChar
  void newline <|> eof
  return word

parserP :: Parser Rule
parserP = do
  char '"'
  c <- letterChar
  char '"'
  return $ Char c

depP :: Parser Rule
depP = do
  a <- some (L.decimal <* optional (char ' '))
  return $ Dep a

orP :: Parser Rule
orP = do
  first <- depP
  string "| "
  Or first <$> depP

populateMap :: [(Int, Rule)] -> Map Int Rule
populateMap = mconcat . fmap go
  where
    go (id, val) = Map.insert id val Map.empty

createParser :: Map Int Rule -> ReadP String
createParser map = createParser' map (map ! 0) <* ReadP.eof
  where
    createParser' m (Char c) = ReadP.string [c]
    createParser' m (Dep xs) = mconcat <$> traverse (createParser' m . (m !)) xs
    createParser' m (Or a b) = createParser' m a Applicative.<|> createParser' m b

solve :: FilePath -> IO [String]
solve fp = do
  f <- readFile fp
  let Just (rules, words) = parseMaybe inputP f
  let map = populateMap rules
  let parser = createParser map
  return $ filter (isParseable parser) words
  where
    isParseable parser = isJust . listToMaybe . ReadP.readP_to_S parser