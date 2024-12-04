module Main where

import Data.List (sort)
import System.IO

processLine :: String -> [Int]
processLine line = map read $ words line

swapLines :: [[Int]] -> [Int] -> [Int] -> ([Int], [Int])
swapLines input listA listB = case input of
  [] -> (sort listA, sort listB)
  (h : t) -> do
    let newA = head h : listA
    let newB = last h : listB
    swapLines t newA newB

map2 :: ((a, a) -> b) -> ([a], [a]) -> [b]
map2 fn input = case input of
  ([], []) -> []
  (h1 : t1, h2 : t2) -> fn (h1, h2) : map2 fn (t1, t2)
  _ -> []

distance :: (Int, Int) -> Int
distance (a, b) = abs $ a - b

mult :: Int -> Int -> Int
mult a b = a * b

part2Process :: ([Int], [Int]) -> [Int]
part2Process (listA, listB) =
  map (\a -> mult a $ length $ filter (== a) listB) listA

part1 :: String -> IO ()
part1 contents = do
  let linesOfFile = lines contents
  let processedLines = map processLine linesOfFile
  let swapped = swapLines processedLines [] []
  let distances = map2 distance swapped
  print $ sum distances

part2 :: String -> IO ()
part2 contents = do
  let linesOfFile = lines contents
  let processedLines = map processLine linesOfFile
  let swapped = swapLines processedLines [] []
  let out = part2Process swapped
  print $ sum out

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  -- handle <- openFile "example.txt" ReadMode
  contents <- hGetContents handle
  part1 contents
  part2 contents
  hClose handle
