{-# LANGUAGE PartialTypeSignatures #-}

module Day21 where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (second))
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Map (Map, (!))
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

type Parser = Parsec Void String

data Dish = Dish
  { ingredients :: Set String,
    allergens :: Set String
  }
  deriving (Show)

dishP :: Parser Dish
dishP = do
  ingredients <- some (some letterChar <* optional space)
  string "(contains "
  allergens <- some letterChar `sepBy` string ", "
  char ')'
  return $ Dish (Set.fromList ingredients) (Set.fromList allergens)

solve :: FilePath -> _
solve fp = do
  f <- readFile fp
  let (Just dishes) = parseMaybe (some (dishP <* (void newline <|> eof))) f
  return $ solve' dishes

simplify :: (Eq a1, Ord a2) => [(a1, Set a2)] -> [(a1, Set a2)]
simplify xs
  | next == xs = xs
  | otherwise = simplify next
  where
    occupied = mconcat . fmap snd . filter ((== 1) . Set.size . snd) $ xs
    next = fmap update xs
    update (x, s)
      | Set.size (s \\ occupied) == 0 = (x, s)
      | otherwise = (x, s \\ occupied)

solve' dishes = intercalate "," . fmap fst . sortBy (compare `on` snd) . fmap (second Set.toList) . simplify $ possibilities
  where
    possibilities = zip (Set.toList remainingIngredients) (fmap ((allAllergens \\) . (map !)) . Set.toList $ remainingIngredients)
    allAllergens = foldMap allergens dishes
    allIngredients = foldMap ingredients dishes
    allergenFree = Set.fromList . fmap fst . Map.toList . Map.filter (== allAllergens) $ map
    remainingIngredients = allIngredients \\ allergenFree
    map = buildMap dishes
    buildMap dishes = Map.unionsWith (<>) . concatMap go $ dishes
      where
        go dish = (\x -> Map.insert x (allergens dish) Map.empty) <$> Set.toList (allIngredients \\ ingredients dish)