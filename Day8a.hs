module Day8a
  ( solve,
  )
where

import Control.Monad.State
    ( MonadState(put, get), evalState, State )
import Data.Array (Array, listArray, (!), (//))
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, some)
import Text.Megaparsec.Char (letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Instruction = Instruction String Int deriving (Show)

type Line = Int

type Accumulator = Int

data GameState = GameState Line Accumulator (Array Int Bool) deriving (Show)

instructionP :: Parser Instruction
instructionP = do
  name <- space >> some letterChar
  number <- space >> L.signed space L.decimal
  return . Instruction name $ number

runInstructions :: Array Int Instruction -> State GameState Accumulator
runInstructions instructions = do
  GameState line accumulator map <- get
  if map ! line
    then return accumulator
    else do
      let updatedMap = map // [(line, True)]
      let instruction = instructions ! line
      put (GameState (line + lineDelta instruction) (accumulator + accumulatorDelta instruction) updatedMap)
      runInstructions instructions

lineDelta :: Instruction -> Int
lineDelta (Instruction "jmp" offset) = offset
lineDelta _ = 1

accumulatorDelta :: Instruction -> Int
accumulatorDelta (Instruction "acc" delta) = delta
accumulatorDelta _ = 0

solve :: FilePath -> IO Accumulator
solve file = do
  lines <- lines <$> readFile file
  let numLines = length lines
  let instructions = listArray (1, length lines) . fromJust . traverse (parseMaybe instructionP) $ lines
  return $ evalState (runInstructions instructions) $ GameState 1 0 (allFalse numLines)

allFalse :: Int -> Array Int Bool
allFalse n = listArray (1, n) (replicate n False)