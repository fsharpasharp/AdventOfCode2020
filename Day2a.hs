module Day2a
  ( solve,
  )
where

import Data.Char (isAlpha, isDigit)
import Text.ParserCombinators.ReadP
    ( char, get, munch1, readP_to_S, skipSpaces )

data PasswordSpec = PasswordSpec
  { letter :: Char,
    minimumTimes :: Int,
    maximumTimes :: Int,
    password :: String
  } 

instance Read PasswordSpec where
  readsPrec _ = readP_to_S $ do
    min <- munch1 isDigit
    char '-'
    max <- munch1 isDigit
    skipSpaces
    letter <- get
    munch1 (not . isAlpha)
    password <- munch1 isAlpha
    return
      PasswordSpec
        { letter = letter,
          minimumTimes = read min :: Int,
          maximumTimes = read max :: Int,
          password = password
        }

validPassword :: PasswordSpec -> Bool
validPassword ps = minimumTimes ps <= counts  && counts <= maximumTimes ps
    where counts = length . filter (== letter ps) $ password ps


solve :: FilePath -> IO Int
solve file = length . filter validPassword . map read . lines <$> readFile file