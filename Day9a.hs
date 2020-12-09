module Day9a
  ( solve,
  )
where

import Data.List (find)

solve :: (Read a, Eq a, Num a) => FilePath -> IO (Maybe ([a], a))
solve file = do
  lines <- fmap read . lines <$> readFile file

  return $ find (\(x, y) -> y `notElem` sums x) . groupLines $ lines

groupLines :: [b] -> [([b], b)]
groupLines [] = []
groupLines lines@(_ : xs) = (preamble, y) : groupLines xs
  where
    preamble = take 25 lines
    (y : _) = drop 25 lines

sums :: Num a => [a] -> [a]
sums [] = []
sums (x : xs) = fmap (+ x) xs ++ sums xs
