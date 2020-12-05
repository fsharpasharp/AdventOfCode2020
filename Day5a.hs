module Day5a
  ( solve,
  )
where

solve :: Integral b => FilePath -> IO b
solve file = do
  f <- readFile file
  return . maximum . map solve' . lines $ f

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