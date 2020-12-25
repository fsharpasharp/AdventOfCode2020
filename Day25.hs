{-# LANGUAGE PartialTypeSignatures #-}

module Day25 where

import Data.List (elemIndex, iterate')
import Data.Maybe (fromJust)

loop :: Integral a => a -> a -> a
loop factor start = (factor * start) `mod` constant
  where
    constant = 20201227

pub1 :: Int
pub1 = 15113849

pub2 :: Int
pub2 = 4206373

encryptionKey :: Int
encryptionKey = iterate' (loop pub2) 1 !! loopSize
  where
    loopSize = fromJust . elemIndex pub1 . iterate' (loop 7) $ 1