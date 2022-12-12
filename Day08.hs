import Data.List

type Grid = [[Int]]
readGrid :: String -> Grid
readGrid = (fmap . fmap) (read . (:[])) . lines

isHorizontallyVisible :: Grid -> (Int, Int) -> Bool
isHorizontallyVisible g (x, y) =
  let row = g !! x
      (left, target : right) = splitAt y row
   in  all (< target) left || all (< target) right

isVisible :: Grid -> (Int, Int) -> Bool
isVisible g (x, y) = 
  isHorizontallyVisible g (x, y) ||
  isHorizontallyVisible (transpose g) (y, x)

linearScenicScore :: Int -> [Int] -> Int
linearScenicScore _ [] = 0
linearScenicScore target (x:xs) = 
  if x >= target
  then 1
  else 1 + linearScenicScore target xs

horizontalScenicScore :: Grid -> (Int, Int) -> Int
horizontalScenicScore g (x, y) =
  let (left, target: right) = splitAt y $ g !! x
  in linearScenicScore target right * linearScenicScore target (reverse left)

scenicScore :: Grid -> (Int, Int) -> Int
scenicScore g (x, y) =
  horizontalScenicScore g (x, y) * horizontalScenicScore (transpose g) (y, x)

main = do
  rawData <- readFile "inputs/day08.txt"
  let grid = readGrid rawData
  let lastRow = length grid - 1
  let lastCol = (length . head) grid - 1
  let allPositions = [(x, y) | x <- [0..lastRow], y <- [0..lastCol]]
  print . length . filter (isVisible grid) $ allPositions
  print . maximum . fmap (scenicScore grid) $ allPositions