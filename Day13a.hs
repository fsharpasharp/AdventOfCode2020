module Day13a where

import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parseMaybe, some, (<|>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

numberP :: Parser [Int]
numberP = some $ do
  x <- L.decimal
  many (char ',' <|> char 'x')
  return x

solve :: FilePath -> IO Int
solve fp = do
  lines <- lines <$> readFile fp
  let time = read . head $ lines :: Int
  let busses = fromJust . parseMaybe numberP $ last lines
  return $ solve' time busses

solve' :: Integral a => a -> [a] -> a
solve' time busses = waitingTime * busID
  where
    (waitingTime, busID) = minimum . flip zip busses . fmap arrival $ busses
    arrival x
      | remaining == 0 = 0
      | otherwise = x - remaining
      where
        remaining = time `mod` x
