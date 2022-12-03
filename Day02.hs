#!/usr/bin/env stack
-- stack script --resolver lts-20.2

import Data.Maybe
import Data.List

data Shape = Rock | Paper | Scissors deriving (Eq)
data Outcome = Win | Draw | Loss deriving (Eq)
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
moveToGetOutcome outcome yourMove =
  fromJust $ find
    (\move -> evalRound (yourMove, move) == outcome)
    [Rock, Paper, Scissors]

readRoundForReal :: String -> Round
readRoundForReal (expectedMove : ' ' : outcome : []) =
  let yourMove = readShape expectedMove
      myMove = moveToGetOutcome (readOutcome outcome) yourMove
  in (yourMove, myMove)

scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

evalRound :: Round -> Outcome
evalRound round = case round of
  (Rock, Paper) -> Win
  (Paper, Scissors) -> Win
  (Scissors, Rock) -> Win
  (yourMove, myMove) ->
    if yourMove == myMove then Draw else Loss

scoreOutcome :: Outcome -> Int
scoreOutcome Win = 6
scoreOutcome Draw = 3
scoreOutcome Loss = 0

scoreRound :: Round -> Int
scoreRound round@(_yourMove, myMove) =
  scoreShape myMove + scoreOutcome (evalRound round)

main = do
  rawData <- readFile "inputs/day02.txt"
  let rawRounds = lines rawData
  print . sum $ fmap (scoreRound . readRound) rawRounds
  print . sum $ fmap (scoreRound . readRoundForReal) rawRounds
