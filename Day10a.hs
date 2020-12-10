module Day10a
  ( solve,
  )
where

import Data.List (sort)

solve :: FilePath -> IO Int
solve file = do
  numbers <- fmap read . lines <$> readFile file
  return $ solve' numbers

solve' :: [Int] -> Int
solve' xs = ones * threes
  where
    sorted = sort xs
    deltas = zipWith (-) (drop 1 sorted) sorted
    ones = (+ 1) . length . filter (== 1) $ deltas
    threes = (+ 1) . length . filter (== 3) $ deltas