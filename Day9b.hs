module Day9b
  ( solve,
  )
where

goal :: Integer
goal = 21806024

solve :: FilePath -> IO Integer
solve file = do
  numbers <- fmap read . lines <$> readFile file
  return $ solve' numbers [] 0

solve' :: [Integer] -> [Integer] -> Integer -> Integer
solve' (x:xs) [] accumulator = solve' xs [x] (accumulator+x)
solve' (x:xs) all@(y:ys) accumulator
  | accumulator == goal = minimum all + maximum all
  | accumulator >= goal = solve' (x:xs) ys (accumulator-y)
  | otherwise = solve' xs (all++[x]) (accumulator+x)