module Day14a where

import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof),
    Parsec,
    optional,
    parseMaybe,
    some,
  )
import Text.Megaparsec.Char (alphaNumChar, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Instruction = Instruction String [(Int, Int)] deriving (Show)

instructionP :: Parser Instruction
instructionP = do
  string "mask = "
  mask <- some alphaNumChar
  newline

  assignments <- some $ do
    string "mem["
    key <- L.decimal
    string "] = "
    val <- L.decimal
    optional newline
    return (key, val)

  return $ Instruction mask assignments

solve :: FilePath -> IO Int
solve fp = do
  f <- readFile fp
  let instructions = fromJust . parseMaybe (some instructionP <* eof) $ f
  return $ solve' instructions

solve' :: Foldable t => t Instruction -> Int
solve' instructions = sum . fmap snd . Map.toList . foldl (flip ($)) Map.empty $updateMap
  where
    updateMap = concatMap insertKeys instructions

insertKeys :: Instruction -> [Map Int Int -> Map Int Int]
insertKeys (Instruction _ []) = []
insertKeys (Instruction m ((x, y) : xs)) = Map.insert x (mask m y) : insertKeys (Instruction m xs)

mask :: String -> Int -> Int
mask x maskee = updated .&. complement zeros
  where
    updated = ones .|. maskee
    ones = parseBinary (keep '1' x)
    zeros = parseBinary (keep '0' x)
    parseBinary = fromJust . parseMaybe (L.binary :: Parser Int)
    keep _ [] = []
    keep c (y : ys)
      | y == c = '1' : keep c ys
      | otherwise = '0' : keep c ys