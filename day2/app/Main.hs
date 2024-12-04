module Main where

import Debug.Trace (trace)
import System.IO
import Text.Printf (printf)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

processLine :: String -> [Int]
processLine input =
  map read $ words input

valid :: (Int, Int) -> Bool -> Bool
valid (h1, h2) isAscending = do
  let diff = if isAscending then h1 - h2 else h2 - h1
  case diff of
    1 -> True
    2 -> True
    3 -> True
    _ -> False

isSafeInner :: [Int] -> Bool -> Bool
isSafeInner input isAscending = case input of
  [] -> True
  [_] -> True
  (h1 : h2 : t) -> do
    let newTail = h2 : t
    valid (h1, h2) isAscending && isSafeInner newTail isAscending

isSafe :: [Int] -> Bool
isSafe input =
  isSafeInner input ((head input - (input !! 1)) >= 0)

part1 :: String -> Int
part1 input = do
  let linesOfFile = lines input
  let processedLines = map processLine linesOfFile
  let safeLines = filter isSafe processedLines
  trace ("ProssedLines: " ++ show processedLines) $ length safeLines

makeSmaller :: [Int] -> Int -> [Int]
makeSmaller input index = do
  let enumerate = zip [0 ..]
  let filterFn (indx, _) = indx /= index
  let (_, out) = enumerate input |> filter filterFn |> unzip
  out

isSafe2 :: [Int] -> Bool
isSafe2 input =
  isSafe input
    || [0 .. length input] |> map (makeSmaller input) |> map isSafe |> or

part2 :: String -> Int
part2 input = do
  let linesOfFile = lines input
  let processedLines = map processLine linesOfFile
  let safeLines = filter isSafe2 processedLines
  length safeLines

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  -- handle <- openFile "example.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
