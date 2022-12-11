import Data.List

startsWithMarkerOfLength :: Int -> [Char] -> Bool
startsWithMarkerOfLength n = (== n) . length . nub . take n

indexOfMarkerOfLength :: Int -> String -> Int
indexOfMarkerOfLength n s =
  if startsWithMarkerOfLength n s
    then n
    else 1 + indexOfMarkerOfLength n (tail s)

indexOfPacketMarker :: String -> Int
indexOfPacketMarker = indexOfMarkerOfLength 4

indexOfMessageMarker :: String -> Int
indexOfMessageMarker = indexOfMarkerOfLength 14

main = do
  rawData <- readFile "inputs/day06.txt"
  print . indexOfPacketMarker $ rawData
  print . indexOfMessageMarker $ rawData