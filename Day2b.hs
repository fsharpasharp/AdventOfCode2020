module Day2b
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
validPassword ps = (==1) . length . filter (== letter ps) $ [letterAt minimumTimes, letterAt maximumTimes]
    where letterAt i = password ps !! (i ps - 1)


solve :: FilePath -> IO Int
solve file = length . filter validPassword . map read . lines <$> readFile file