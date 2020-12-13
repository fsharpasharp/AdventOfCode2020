{-# LANGUAGE OverloadedStrings #-}

module Day13b where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (sort)
import Data.Bifunctor ( Bifunctor(first) )


solve :: FilePath -> IO Int
solve fp = do
  lines <- T.lines <$> T.readFile fp
  let rawBusses = fmap T.unpack . T.splitOn "," $ last lines
  let busses = fmap (first read) . filter ((/= "x") . fst) . flip zip [0..] $ rawBusses
  return . solve' . reverse .  sort $ busses

solve' :: Integral b => [(b, b)] -> b
solve' busses = search busses 0 1

search :: Integral b => [(b, b)] -> b -> b -> b
search [] n _ = n
search again@((freq,delay):xs) n inc
  | (n+delay) `mod` freq == 0 = search xs n (inc*freq)
  | otherwise = search again (n+inc) inc