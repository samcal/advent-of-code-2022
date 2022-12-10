#!/usr/bin/env stack
-- stack script --resolver lts-20.2 --package split --package lens

import Control.Lens
import Data.Char
import Data.List
import Data.List.Lens
import Data.List.Split
import Data.Maybe

readStateLine :: String -> [[Char]]
readStateLine [] = []
readStateLine cs@('[' : c : ']' : _) = [c] : readStateLine (drop 4 cs)
readStateLine cs@(' ' : _) = [] : readStateLine (drop 4 cs)
readStateLine e = error e

type StackState = [[Char]]
readInitialState :: String -> StackState
readInitialState =
  fmap concat .
  transpose .
  fmap (readStateLine) .
  init .
  lines

-- Move 0 from 1 to 2
type Instruction = (Int, Int, Int)
readInstruction :: String -> Instruction
readInstruction s =
  let [_, count, _, source, _, target] = words s
  in (read count, (read source) - 1, (read target) - 1)

applyInstruction :: Instruction -> StackState -> StackState
applyInstruction (count, source, target) state =
  let sourceStack = state ^. (ix source)
      targetStack = state ^. (ix target)
      (cratesToMove, remainingStack) = splitAt count sourceStack
      stateAfterRemoving = set (ix source) remainingStack state
      stateAfterReplacing =
        set
        (ix target)
        (reverse cratesToMove ++ targetStack)
        stateAfterRemoving
  in stateAfterReplacing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

main = do
  rawData <- readFile "inputs/sample05.txt"
  let [rawState, rawInstructions] = splitOn "\n\n" rawData
  let initialState = readInitialState rawState
  let instructions = fmap readInstruction $ lines rawInstructions

  let endState = foldr applyInstruction initialState instructions

  print . fmap (fromMaybe ' ' . safeHead) $ endState
