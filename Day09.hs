#!/usr/bin/env stack
-- stack script --resolver lts-20.2

import Data.List

type Position = (Int, Int)
type Rope = [Position] -- (head, tail)

data Instruction =
  MoveUp |
  MoveRight |
  MoveDown |
  MoveLeft
  deriving (Show)
readInstruction :: String -> [Instruction]
readInstruction s = case words s of
  ["U", amount] -> replicate (read amount) MoveUp
  ["R", amount] -> replicate (read amount) MoveRight
  ["D", amount] -> replicate (read amount) MoveDown
  ["L", amount] -> replicate (read amount) MoveLeft

moveHead :: Position -> Instruction -> Position
moveHead (x, y) MoveUp = (x, y + 1)
moveHead (x, y) MoveDown = (x, y - 1)
moveHead (x, y) MoveRight = (x + 1, y)
moveHead (x, y) MoveLeft = (x - 1, y)

moveTails :: Rope -> Rope
moveTails [] = []
moveTails [_] = []
moveTails (head@(hx, hy) : tail@(tx, ty) : tails) =
  let tail' =
        if isTailTooFar (head, tail)
        then case (compare hx tx, compare hy ty) of
          (EQ, EQ) -> (hx, hy)
          (EQ, GT) -> (hx, hy - 1)
          (EQ, LT) -> (hx, hy + 1)
          (GT, EQ) -> (hx - 1, hy)
          (LT, EQ) -> (hx + 1, hy)
          (GT, GT) -> (tx + 1, ty + 1)
          (GT, LT) -> (tx + 1, ty - 1)
          (LT, GT) -> (tx - 1, ty + 1)
          (LT, LT) -> (tx - 1, ty - 1)
        else tail
  in tail' : (moveTails $ tail' : tails)

isTailTooFar :: (Position, Position) -> Bool
isTailTooFar ((hx, hy), (tx, ty)) =
  abs (hx - tx) > 1 || abs (hy - ty) > 1

moveRope :: Rope -> Instruction -> Rope
moveRope (headPos : tails) instruction =
  let headPos' = moveHead headPos instruction
  in headPos': (moveTails $ headPos' : tails)

ropePositions :: Rope -> [Instruction] -> [Rope]
ropePositions _ [] = []
ropePositions rope (i:is) =
  let newRope = moveRope rope i
  in newRope : ropePositions newRope is

ropeOfLength :: Int -> Rope
ropeOfLength n = replicate n (0, 0)

main = do
  rawData <- readFile "inputs/day09.txt"
  let instructions = lines rawData >>= readInstruction
  print . length . nub . fmap last . ropePositions (ropeOfLength 2) $ instructions
  print . length . nub . fmap last . ropePositions (ropeOfLength 10) $ instructions
