{-# LANGUAGE OverloadedStrings #-}
module Day6a
  ( solve,
  )
where

import Data.List ( nub )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


solve :: FilePath -> IO Int
solve file = do
  f <- T.readFile file
  return .  solve' $ f


solve' :: Text -> Int
solve' = sum . fmap (length . nub. T.unpack . T.replace "\n" "") . T.splitOn "\n\n"


