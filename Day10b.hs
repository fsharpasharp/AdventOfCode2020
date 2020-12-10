module Day10b
  ( solve,
  )
where

import Data.Array
import Data.List (sort)

solve :: FilePath -> IO Int
solve file = do
  numbers <- fmap read . lines <$> readFile file
  return $ solve' numbers

solve' :: [Int] -> Int
solve' xs = dp sorted (listArray (-2, upper) (0:0:1 : replicate upper 0))
  where
    sorted = sort xs
    upper = maximum sorted - 1

dp :: [Int] -> Array Int Int -> Int
dp [x] ar = sum $ (ar !) <$> [x-3, x-2, x-1]
dp (x:xs) ar = dp xs (ar // [(x, prevSum)])
  where prevSum = sum $ (ar !) <$> [x-3, x-2, x-1]
