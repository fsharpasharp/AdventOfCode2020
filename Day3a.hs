module Day3a
  ( solve,
  )
where

solve :: FilePath -> IO Int
solve file = length . filter (== '#') . solve' 3 . tail . map cycle . lines <$> readFile file
  where
    solve' _ [] = []
    solve' n (x : xs) = x !! (n * 3) : solve' (n + 1) xs