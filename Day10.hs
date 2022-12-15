#!/usr/bin/env stack
-- stack script --resolver lts-20.2

type Register = Int

data Instruction = Noop | AddX Int deriving (Show)
readInstruction :: String -> Instruction
readInstruction s = case words s of
  ["noop"] -> Noop
  ["addx", n] -> AddX $ read n

applyInstructions :: [Instruction] -> Register -> [Register]
applyInstructions [] _ = []
applyInstructions (i:is) reg = case i of
  Noop -> reg : applyInstructions is reg
  AddX n -> reg : reg : applyInstructions is (reg + n)

interestingValues :: [Register] -> [Register]
interestingValues as =
  fmap (\x -> (as !! (x-1)) * x) [20, 60, 100, 140, 180, 220]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : (chunk n $ drop n xs)

renderCRT :: [Register] -> String
renderCRT =
  unlines .
  chunk 40 .
  fmap (\(i, reg) -> if abs (reg - i) <= 1 then '#' else '.') .
  zip (concat $ repeat [0..39])

main = do
  rawData <- readFile "inputs/day10.txt"
  let instructions = fmap readInstruction $ lines rawData
  let cycleValues = applyInstructions instructions 1
  print . sum . interestingValues $ cycleValues
  putStrLn . renderCRT $ cycleValues
