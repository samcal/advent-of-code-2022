#!/usr/bin/env stack
-- stack script --resolver lts-20.2

data Shape = Rock | Paper | Scissors deriving (Show, Eq)
data Outcome = Win | Draw | Loss deriving (Show)
type Round = (Shape, Shape)

readShape :: Char -> Shape
readShape 'A' = Rock
readShape 'X' = Rock
readShape 'B' = Paper
readShape 'Y' = Paper
readShape 'C' = Scissors
readShape 'Z' = Scissors

readRound :: String -> Round
readRound (yourMove : ' ' : myMove : []) =
  (readShape yourMove, readShape myMove)

readOutcome :: Char -> Outcome
readOutcome 'X' = Loss
readOutcome 'Y' = Draw
readOutcome 'Z' = Win

moveToGetOutcome :: Outcome -> Shape -> Shape
moveToGetOutcome Win Rock = Paper
moveToGetOutcome Win Paper = Scissors
moveToGetOutcome Win Scissors = Rock
moveToGetOutcome Draw shape = shape
moveToGetOutcome Loss shape =
  (moveToGetOutcome Win . moveToGetOutcome Win) shape

readRoundForReal :: String -> Round
readRoundForReal (expectedMove : ' ' : outcome : []) =
  let yourMove = readShape expectedMove
  in (yourMove, moveToGetOutcome (readOutcome outcome) yourMove)

scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

evalRound :: Round -> Outcome
evalRound (yourMove, myMove)
  | yourMove == myMove = Draw
  | yourMove == Rock && myMove == Paper = Win
  | yourMove == Paper && myMove == Scissors = Win
  | yourMove == Scissors && myMove == Rock = Win
  | otherwise = Loss

scoreOutcome :: Outcome -> Int
scoreOutcome Win = 6
scoreOutcome Draw = 3
scoreOutcome Loss = 0

scoreRound :: Round -> Int
scoreRound round =
  let outcome = evalRound round
  in scoreShape (snd round) + scoreOutcome outcome

main = do
  rawData <- readFile "inputs/day02.txt"
  let rawRounds = lines rawData
  print . sum $ fmap (scoreRound . readRound) rawRounds
  print . sum $ fmap (scoreRound . readRoundForReal) rawRounds
