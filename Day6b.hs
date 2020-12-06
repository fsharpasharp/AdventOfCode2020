{-# LANGUAGE OverloadedStrings #-}
module Day6b
  ( solve,
  )
where

import Data.List (intersect)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


solve :: FilePath -> IO Int
solve file = do
  f <- T.readFile file
  return .  solve' $ f

solve' :: Text -> Int
solve' = sum . fmap (length . foldr1 intersect . fmap T.unpack . T.lines) . T.splitOn "\n\n"


