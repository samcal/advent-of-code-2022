#!/usr/bin/env stack
-- stack script --resolver lts-20.2

import Data.Ord
import Data.List
import Data.List.Split

readInventories :: String -> [[Int]]
readInventories = readNestedInts . splitOnBlankLines
  where
    splitOnBlankLines :: String -> [[String]]
    splitOnBlankLines = splitOn [""] . lines

    readNestedInts :: [[String]] -> [[Int]]
    readNestedInts = (fmap . fmap) read

main = do
  rawData <- readFile "inputs/day01.txt"
  let inventories = readInventories rawData
  let sortedTotals = sortBy (comparing Down) (fmap sum inventories)
  print . head $ sortedTotals
  print . sum . take 3 $ sortedTotals
