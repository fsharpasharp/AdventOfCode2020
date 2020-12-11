module Day11a
  ( solve,
  )
where

import Data.Array ( Array, elems, listArray, (!), (//), assocs )
import Control.Monad (guard)


type Pos = (Int,Int)

solve :: FilePath -> IO Int
solve file = do
  lines <- lines <$> readFile file
  let rows = length lines
  let columns = length . head $ lines

  let extraRow = replicate (columns+2) '.'
  let updatedLines = extraRow: fmap (\x -> '.':x ++ ".") lines ++ [extraRow]
  let matrix = listArray ((0,0), (rows+1, columns+1)) (concat updatedLines)
  return $ solve' matrix


surrounding :: Pos -> [Pos]
surrounding (y,x) = do
  i <- [-1,0,1]
  j <- [-1,0,1]
  guard (i /= 0 || j /= 0)
  return (y+i, x+j)


occupiedSurrounding :: Array Pos Char -> Pos -> Int
occupiedSurrounding mat = length . filter (=='#') . fmap (mat !) . surrounding

solve' :: Array Pos Char -> Int
solve' mat  
  | newMat /= mat = solve' newMat 
  | otherwise = length . filter (== '#') . elems $ mat
  where newMat = mat // (concatMap (update mat) . assocs $ mat)

update :: Array Pos Char -> (Pos,Char) -> [(Pos,Char)]
update mat (pos, '#')
  | occupiedSurrounding mat pos >= 4 = [(pos, 'L')]
update mat (pos, 'L')
  | occupiedSurrounding mat pos == 0 = [(pos, '#')]
update _  _ = []
