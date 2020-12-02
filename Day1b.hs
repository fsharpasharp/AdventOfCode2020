module Day1b
  ( solve,
  )
where

solve :: FilePath -> IO Int
solve file = tripleProduct . fmap read . lines <$> readFile file

tripleProduct :: [Int] -> Int
tripleProduct (x : xs) =
  case tripleProduct' xs x of
    Nothing -> tripleProduct xs
    Just x -> x

tripleProduct' :: [Int] -> Int -> Maybe Int
tripleProduct' [] _ = Nothing
tripleProduct' (x : xs) y
  | complement `elem` xs = Just $ x * y * complement
  | otherwise = tripleProduct' xs y
  where
    complement = 2020 - y - x
