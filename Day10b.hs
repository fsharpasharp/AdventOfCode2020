module Day10b
  ( solve,
  )
where

import Data.Array ( (!), (//), listArray, Array )
import Data.List (sort)

solve :: FilePath -> IO Int
solve file = do
  numbers <- fmap read . lines <$> readFile file
  return $ solve' numbers

solve' :: [Int] -> Int
solve' xs = dp sorted (listArray (-2, upper) (0:0:1 : replicate upper 0))
  where
    sorted = 0:sort xs
    upper = maximum xs - 1

dp :: [Int] -> Array Int Int -> Int
dp (x:xs) ar = case xs of
  [] -> prevSum
  _ -> dp xs (ar // [(x, prevSum)])
  where prevSum = sum $ (ar !) <$> [x-3, x-2, x-1]