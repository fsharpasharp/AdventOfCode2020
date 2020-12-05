module Day4b
  ( solve,
  )
where

import Control.Monad ( guard, void )
import Data.Maybe (fromJust, isJust)
import Data.Void ( Void ) 
import Text.Megaparsec
    ( optional,
      (<|>),
      parseMaybe,
      satisfy,
      choice,
      count,
      some,
      Parsec )
import Text.Megaparsec.Char
    ( alphaNumChar,
      char,
      digitChar,
      letterChar,
      newline,
      space,
      string )
import qualified Text.Megaparsec.Char.Lexer as L

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

numParserWithLimit :: Int -> Int -> Parser ()
numParserWithLimit min max = do
  x <- L.decimal
  guard (min <= x && x <= max)

hgtP :: Parser ()
hgtP = do
  x <- L.decimal
  y <- string "cm" <|> string "in"
  case y of
    "cm" -> guard (150 <= x && x <= 193)
    "in" -> guard (59 <= x && x <= 76)

ls :: [(String, Parser ())]
ls =
  [ ("byr", numParserWithLimit 1920 2002),
    ("iyr", numParserWithLimit 2010 2020),
    ("eyr", numParserWithLimit 2020 2030),
    ("hgt", hgtP),
    ("hcl", char '#' >> (void . count 6 $ alphaNumChar)),
    ( "ecl",
      void $
        Text.Megaparsec.choice
          [ string "amb",
            string "blu",
            string "brn",
            string "gry",
            string "grn",
            string "hzl",
            string "oth"
          ]
    ),
    ("pid", void $ count 9 digitChar)
  ]

validPassport :: [(String, String)] -> Maybe [()]
validPassport dic = mapM (\(key, p) -> lookup key dic >>= parseMaybe p) ls

solve :: FilePath -> IO Int
solve file = do
  f <- readFile file
  return $ length . filter isJust . map validPassport . fromJust . parseMaybe pairsP $ f