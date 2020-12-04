module Day4a
  ( solve,
    pairP,
  )
where

import Control.Monad
import Data.Maybe (fromJust, isJust)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read

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

mandatoryKeys :: [String]
mandatoryKeys =
  [ "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  ]

validPassport :: [(String, String)] -> [Maybe String]
validPassport dic = lookup <$> mandatoryKeys <*> [dic]

solve file = do
  f <- readFile file
  return $ length . filter (all isJust) . map validPassport . fromJust . parseMaybe pairsP $ f
