module Day4b
  ( solve,
    pairP,
  )
where

import Control.Monad
import Data.Maybe (isJust, fromJust)
import Data.Void
import Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

pairsP :: Parser [[(String, String)]]
pairsP = some $ do
  pair <- some pairP
  optional (void newline)
  return pair

pairP :: Parser (String, String)
pairP = do
  key <- some letterChar
  void . char $ ':'
  value <- some . satisfy $ (`notElem` [' ', '\n'])
  void newline <|> void space
  return (key, value)


newtype Height = Height (Int, String) deriving Show

heightP :: Parser (Maybe Height)
heightP = do
  val <- some digitChar 
  unit <- string "cm" <|> string "in"
  return $ height (read val) unit

height :: Int -> String -> Maybe Height
height x "in" 
  | 59 <= x && x <= 76 = Just $ Height (x, "in")
height x "cm"
  | 150 <= x && x <= 193 = Just $ Height (x, "cm")
height _ _ = Nothing


hclP :: Parser String
hclP = do
  char '#'
  count 6 alphaNumChar

pidP :: Parser String
pidP = do
  count 9 digitChar


validPassport :: [(String,String)] -> Bool
validPassport dic = isJust $ do
  yearLimit "byr" 1920 2002
  yearLimit "iyr" 2010 2020
  yearLimit "eyr" 2020 2030
  join $ lookup "hgt" dic >>= parseMaybe heightP
  lookup "hcl" dic >>= parseMaybe hclP
  ecl <- lookup "ecl" dic
  guard (ecl `elem` validColors)
  lookup "pid" dic >>= parseMaybe pidP
  where yearLimit key min max = do
          x <- lookup key dic >>= readMaybe :: Maybe Int
          guard (min <= x && x <= max)
          return x
        validColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

solve file = do
  f <- readFile file
  return $ length . filter validPassport . fromJust . parseMaybe pairsP $ f
