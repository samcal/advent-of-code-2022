import Data.List
import Data.Maybe

type Rucksack = ([Char], [Char])

readRucksack :: String -> Rucksack
readRucksack s =
  let compartmentSize = length s `div` 2
  in (take compartmentSize s, drop compartmentSize s)

findDuplicateItem :: Rucksack -> Char
findDuplicateItem (firstCompartment, secondCompartment) = 
  head $ intersect firstCompartment secondCompartment

itemPriority :: Char -> Int
itemPriority item = (+1) . fromJust $ elemIndex item (['a'..'z'] ++ ['A'..'Z'])

groupIntoThrees :: [String] -> [[String]]
groupIntoThrees [] = []
groupIntoThrees xs = take 3 xs : groupIntoThrees (drop 3 xs)

findCommonItem :: [String] -> Char
findCommonItem [firstElf, secondElf, thirdElf] = head . intersect firstElf $ intersect secondElf thirdElf

main = do
  rawData <- readFile "inputs/day03.txt"
  let elves = lines rawData
  print . sum $ fmap (itemPriority . findDuplicateItem . readRucksack) elves
  print . sum . fmap (itemPriority . findCommonItem) $ groupIntoThrees elves
  