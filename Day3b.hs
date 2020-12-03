module Day3b
  ( solve,
  )
where

slopes :: [(Int, Int)]
slopes =
  [ (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)
  ]

solve :: FilePath -> IO Int
solve file = do
  f <- readFile file
  return $ product $ trees <$> slopes <*>  [map cycle . lines $ f]

trees :: (Int, Int) -> [[Char]] -> Int
trees (right, down) = length . filter (== '#') . trees' right
  where
    trees' n xss = case drop down xss of
      [] -> []
      now -> head now !! n : trees' (n + right) now