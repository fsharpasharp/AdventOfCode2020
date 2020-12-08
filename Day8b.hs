module Day8b
  ( solve,
  )
where

import Control.Monad (guard)
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
  )
import Data.Array (Array, Ix, listArray, (!), (//))
import Data.Array.Base (assocs)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, some)
import Text.Megaparsec.Char (letterChar, space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Instruction = Instruction String Int deriving (Show, Eq)

type Line = Int

type Accumulator = Int

data GameState = GameState Line Accumulator (Array Int Bool) deriving (Show)

data Status = Terminated | Loop deriving (Show)

data Result = Result Status Accumulator deriving (Show)

instructionP :: Parser Instruction
instructionP = do
  name <- space >> some letterChar
  number <- space >> L.signed space L.decimal
  return . Instruction name $ number

runInstructions :: Array Int Instruction -> State GameState Result
runInstructions instructions = do
  GameState line accumulator map <- get
  if line == length instructions + 1
    then return $ Result Terminated accumulator
    else
      if map ! line
        then return $ Result Loop accumulator
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

solve :: FilePath -> IO (Maybe Result)
solve file = do
  lines <- lines <$> readFile file
  let numLines = length lines
  let instructions = listArray (1, numLines) . fromJust . traverse (parseMaybe instructionP) $ lines
  let modifiedInstructions = modifyInstructions instructions
  return . find onlyTerminated . fmap (solve' numLines) $ modifiedInstructions
  where
    solve' numLines x = evalState (runInstructions x) $ GameState 1 0 (allFalse numLines)
    onlyTerminated (Result Terminated _) = True
    onlyTerminated _ = False

allFalse :: Int -> Array Int Bool
allFalse n = listArray (1, n) (replicate n False)

modifyInstructions :: Ix i => Array i Instruction -> [Array i Instruction]
modifyInstructions instructions = do
  (index, old) <- assocs instructions
  let new = opposite old
  guard (new /= old)
  return $ instructions // [(index, new)]

opposite :: Instruction -> Instruction
opposite (Instruction "nop" o) = Instruction "jmp" o
opposite (Instruction "jmp" o) = Instruction "nop" o
opposite x = x