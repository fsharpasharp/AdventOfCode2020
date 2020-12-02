module Day1a
    ( solve
    ) where

solve :: FilePath -> IO Int
solve file = solve' . fmap read . lines <$> readFile file

solve' (x:xs)
    | complement `elem` xs = x * complement
    | otherwise = solve' xs
    where complement = 2020-x