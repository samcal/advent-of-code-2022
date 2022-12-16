#!/usr/bin/env stack
-- stack script --resolver lts-20.2 --package lens --package split

{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.List
import Data.List.Split
import Data.Ord

type WorryLevel = Int
type Operation = WorryLevel -> WorryLevel
type RunTest = WorryLevel -> Int
data Monkey = Monkey {
  _items :: [WorryLevel],
  _operation :: Operation,
  _divisibility :: Int,
  _runTest :: RunTest,
  _inspectionCount :: Int
}
$(makeLenses ''Monkey)

readItems :: String -> [WorryLevel]
readItems s =
  let [_, itemsStr] = splitOn ": " s
  in fmap read $ splitOn ", " itemsStr

readOperation :: String -> Operation
readOperation s =
  let [_, equationStr] = splitOn " = " s
      [lhsStr, operatorStr, rhsStr] = words equationStr
      operator = case operatorStr of
        "+" -> (+)
        "*" -> (*)
      getLhs n = if lhsStr == "old" then n else read lhsStr
      getRhs n = if rhsStr == "old" then n else read rhsStr
  in (\n -> (getLhs n) `operator` (getRhs n))

readDivisibility :: String -> Int
readDivisibility s = case words s of
  ["Test:", "divisible", "by", n] -> read n

readRunTest :: String -> String -> String -> RunTest
readRunTest testLine trueLine falseLine =
  let
    boolTest = (\level -> level `mod` (readDivisibility testLine) == 0)
    ["If", "true:", "throw", "to", "monkey", trueTarget] = words trueLine
    ["If", "false:", "throw", "to", "monkey", falseTarget] = words falseLine
  in \level -> if boolTest level then read trueTarget else read falseTarget

readMonkeyLines :: [String] -> Monkey
readMonkeyLines [_, itemsLine, operationLine, testLine, trueLine, falseLine] =
  Monkey
    (readItems itemsLine)
    (readOperation operationLine)
    (readDivisibility testLine)
    (readRunTest testLine trueLine falseLine)
    0

monkeysMod :: [Monkey] -> Int
monkeysMod = foldr (*) 1 . fmap _divisibility

data WorryManagementStrategy = DivideByThree | DivisibilityProduct

type TroopState = ([Monkey], Int)
tickTroopState :: WorryManagementStrategy -> TroopState -> TroopState
tickTroopState strategy (monkeys, cursor) =
  let (Just currentMonkey) = monkeys ^? ix cursor
  in case currentMonkey ^. items of
    [] -> (monkeys, (cursor + 1) `mod` (length monkeys))
    (currentItem: rest) ->
      let worryLevelAfterOperation = (currentMonkey ^. operation) currentItem
          newWorryLevel = case strategy of
            DivideByThree -> worryLevelAfterOperation `div` 3
            DivisibilityProduct -> worryLevelAfterOperation `mod` (monkeysMod monkeys)
          destination = (currentMonkey ^. runTest) newWorryLevel
          monkeys' = set (ix cursor . items) rest monkeys
          monkeys'' = over (ix cursor . inspectionCount) (+1) monkeys'
      in  (over (ix destination . items) (++ [newWorryLevel]) monkeys'', cursor)

tickFullRound :: WorryManagementStrategy -> TroopState -> TroopState
tickFullRound strategy state =
  let stateList = iterate (tickTroopState strategy) state
      (lastMonkeys, _) = last .
        takeWhile ((/=0) . snd) .
        dropWhile ((==0) . snd) $
        stateList
  in (lastMonkeys, 0)

showMonkeys :: [Monkey] -> String
showMonkeys =
  unlines .
  fmap (\(i, monkey) ->
    "Monkey " ++ show i ++ ": " ++ show (monkey ^. items)
    ) .
  zip [0..]

showInspections :: [Monkey] -> String
showInspections =
  unlines .
  fmap (\(i, monkey) ->
    "Monkey " ++ show i ++ " inspected items " ++ show (monkey ^. inspectionCount) ++ " times."
    ) .
  zip [0..]

monkeyBusiness (monkeys, _) =
  foldr (*) 1 .
  take 2 .
  sortBy (comparing Down) $
  fmap _inspectionCount monkeys


main = do
  rawData <- readFile "inputs/day11.txt"
  let monkeys = fmap (readMonkeyLines . lines) $ splitOn "\n\n" rawData
  let initialState = (monkeys, 0)
  print . monkeyBusiness $ (iterate (tickFullRound DivideByThree) initialState) !! 20
  print . monkeyBusiness $ (iterate (tickFullRound DivisibilityProduct) initialState) !! 10000

