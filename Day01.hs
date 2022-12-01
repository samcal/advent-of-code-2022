#!/usr/bin/env stack
-- stack --resolver lts-18.5 script

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
  rawData <- readFile "inputs/sample01.txt"
  let inventories = readInventories rawData
  let sortedTotals = sortBy (comparing Down) (fmap sum inventories)
  print . head $ sortedTotals
  print . sum . take 3 $ sortedTotals
