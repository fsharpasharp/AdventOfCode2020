module Day11bfix
  ( solve,
  )
where

import Data.Array (Array, assocs, elems, listArray, (!), (//))
import Data.List (findIndex)

type Pos = (Int, Int)

rows :: Int
rows = 99
columns :: Int
columns = 98

solve :: FilePath -> IO Int
solve file = do
  lines <- lines <$> readFile file
  let matrix = listArray ((0, 0), (rows-1, columns-1)) (concat lines)
  return $ solve' matrix

directions :: [(Int,Int)]
directions = [(i,j) | i <- [-1,0,1], j <- [-1,0,1], i /= 0 || j /= 0]

surrounding :: Array Pos Char -> Pos -> [Pos]
surrounding mat pos = concatMap seenChair . fmap (vision pos) $ directions
  where seenChair xs = case findIndex (/= '.') . fmap (mat !) $ xs of
          Nothing -> []
          Just x -> [xs !! x]


vision :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
vision (yStart,xStart) (yDir,xDir) = takeWhile validValues [(yStart+yDir*i,xStart+xDir*i) | i <- [1..]]
  where validValues (y,x) = 0 <= x && x < columns &&
                            0 <= y && y < rows

occupiedSurrounding :: Array Pos Char -> Pos -> Int
occupiedSurrounding mat = length . filter (== '#') . fmap (mat !) . surrounding mat

solve' :: Array Pos Char -> Int
solve' mat
  | newMat /= mat = solve' newMat
  | otherwise = length . filter (== '#') . elems $ mat
  where
    newMat = mat // (concatMap (update mat) . assocs $ mat)

update :: Array Pos Char -> (Pos, Char) -> [(Pos, Char)]
update mat (pos, '#')
  | occupiedSurrounding mat pos >= 5 = [(pos, 'L')]
update mat (pos, 'L')
  | occupiedSurrounding mat pos == 0 = [(pos, '#')]
update _ _ = []
