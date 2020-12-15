module Day14b where

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
  return $ sum . fmap snd . Map.toList .  foldl (flip ($)) Map.empty . concatMap solve' $ instructions


solve' :: Instruction -> [Map Int Int -> Map Int Int]
solve' (Instruction _ []) = []
solve' (Instruction mask ((key, val) : xs)) = 
  fmap (`Map.insert` val) (allCombinations (address mask key)) ++  solve' (Instruction mask xs)


allCombinations :: String -> [Int]
allCombinations = allCombinations' 0

allCombinations' :: Num a => a -> [Char] -> [a]
allCombinations' n [] = [n]
allCombinations' n (c : xs) = case c of
  '0' -> withZero
  '1' -> withOne
  'X' -> withOne ++ withZero
  where
    withOne = allCombinations' (2 * n + 1) xs
    withZero = allCombinations' (2 * n) xs

address :: [Char] -> Int -> [Char]
address x addressee = zipWith address' x (replicate (length x - (length . toBinary $ addressee)) '0' ++ toBinary addressee)
  where
    address' '1' _ = '1'
    address' 'X' _ = 'X'
    address' _ c = c

toBinary :: Int -> String
toBinary 0 = ""
toBinary x
  | bit == 1 = toBinary (x `div` 2) ++ "1"
  | otherwise = toBinary (x `div` 2) ++ "0"
  where
    bit = x `mod` 2
