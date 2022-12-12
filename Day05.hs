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
  fmap readStateLine .
  init .
  lines

-- Move 0 from 1 to 2
type Instruction = (Int, Int, Int)
readInstruction :: String -> Instruction
readInstruction s =
  let [_, count, _, source, _, target] = words s
  in (read count, (read source) - 1, (read target) - 1)

data Mover = CrateMover9000 | CrateMover9001
applyInstruction :: Mover -> StackState -> Instruction -> StackState
applyInstruction mover state (count, source, target) =
  let sourceStack = state ^. (ix source)
      targetStack = state ^. (ix target)
      (cratesToMove, remainingStack) = splitAt count sourceStack
      stateAfterRemoving = set (ix source) remainingStack state
      orderedCratesToMove = case mover of
        CrateMover9000 -> reverse cratesToMove
        CrateMover9001 -> cratesToMove
      stateAfterReplacing =
        set
        (ix target)
        (orderedCratesToMove ++ targetStack)
        stateAfterRemoving
  in stateAfterReplacing

main = do
  rawData <- readFile "inputs/day05.txt"
  let [rawState, rawInstructions] = splitOn "\n\n" rawData
  let initialState = readInitialState rawState
  let instructions = fmap readInstruction $ lines rawInstructions

  print . fmap head $ foldl' (applyInstruction CrateMover9000) initialState instructions
  print . fmap head $ foldl' (applyInstruction CrateMover9001) initialState instructions
