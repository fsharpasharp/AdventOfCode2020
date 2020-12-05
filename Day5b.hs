module Day5b
  ( solve,
    binarySearch,
  )
where

import Data.List (sort)
solve :: Integral b => FilePath -> IO b
solve file = do
  f <- readFile file
  return . findMissing . sort . map solve' . lines $ f

findMissing :: (Eq a, Num a) => [a] -> a
findMissing (x:y:ys)
    | x + 1 /= y = y
    | otherwise = findMissing (y:ys)

solve' :: Integral a => [Char] -> a
solve' bsp = 8 * row + col
  where
    (rowBSP, colBSP) = splitAt 7 bsp
    row = binarySearch 0 127 rowBSP
    col = binarySearch 0 8 colBSP

binarySearch :: Integral a => a -> a -> [Char] -> a
binarySearch lower _ [] = lower
binarySearch lower upper (x : xs)
  | x `elem` ['F', 'L'] = binarySearch lower (upper - halfRange) xs
  | x `elem` ['B', 'R'] = binarySearch (lower + halfRange) upper xs
  where
    halfRange = ceiling $ fromIntegral (upper - lower) / 2