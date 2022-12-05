type Range = (Int, Int)

splitIntoPairWithSeparator :: Char -> String -> (String, String)
splitIntoPairWithSeparator c s =
  let before = takeWhile (/= c) s
      after = tail $ dropWhile (/= c) s
  in (before, after)

readRange :: String -> Range
readRange s = let (start, end) = splitIntoPairWithSeparator '-' s in (read start, read end)

readRangePair :: String -> (Range, Range)
readRangePair s = 
  let (first, second) = splitIntoPairWithSeparator ',' s
  in (readRange first, readRange second)

completelyOverlaps :: (Range, Range) -> Bool
completelyOverlaps ((start1, end1), (start2, end2))
  | start1 <= start2 && end1 >= end2 = True
  | start2 <= start1 && end2 >= end1 = True
  | otherwise = False

partiallyOverlaps :: (Range, Range) -> Bool
partiallyOverlaps ((start1, end1), (start2, end2))
  | start1 <= end2 && start2 <= end1 = True
  | start2 <= end1 && start1 <= end2 = True
  | otherwise = False

main = do
  rawData <- readFile "inputs/day04.txt"
  let rangePairs = fmap readRangePair (lines rawData)
  print . length $ filter completelyOverlaps rangePairs
  print . length $ filter partiallyOverlaps rangePairs