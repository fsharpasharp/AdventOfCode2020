module Day7a
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

parseLine :: Parser (Bag, [Bag])
parseLine = do
  bagParent <- bagP
  wordP -- bags
  wordP -- contain
  bags <- space >> (string "no other bags." $> []) <|> some containedBagP
  return (bagParent, bags)

containedBagP :: Parser Bag
containedBagP = do
  space >> L.decimal
  bag <- bagP
  wordP -- bag(s)
  char '.' <|> char ','
  return bag

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
    Just xs -> return $ countTerminal xs - 1
    Nothing -> error "Parsing error"

terminal :: Bag
terminal = Bag "shiny" "gold"

countTerminal :: [(Bag, [Bag])] -> Int
countTerminal xs = length . filter (descendantOf xs terminal . fst) $ xs

-- TODO: Memoize or start from the shiny gold bag.
descendantOf :: [(Bag, [Bag])] -> Bag -> Bag -> Bool
descendantOf m e o 
 | e == o = True
 | otherwise = or . fmap (descendantOf m e) . fromJust . lookup o $ m 