module Day9a
  ( solve,
  )
where

import Data.List (find)

solve :: (Read b, Eq b, Num b) => FilePath -> IO (Maybe b)
solve file = do
  lines <- fmap read . lines <$> readFile file
  return $ snd <$> (find (\(x, y) -> y `notElem` sums x) . groupLines $ lines)

groupLines :: [b] -> [([b], b)]
groupLines [] = []
groupLines lines@(_ : xs) = (preamble, lines !! 25) : groupLines xs
  where
    preamble = take 25 lines

sums :: Num a => [a] -> [a]
sums [] = []
sums (x : xs) = fmap (+ x) xs ++ sums xs