module Main where

-- or TDFA or PCRE or ...
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import System.IO
import Text.Regex.Base
import Text.Regex.TDFA

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

getNums :: String -> Maybe (Int, Int)
getNums input = do
  case input =~ "mul\\(([0-9][0-9]?[0-9]?),([0-9][0-9]?[0-9]?)\\)" :: (String, String, String, [String]) of
    (_, _, _, [a, b]) -> Just (read a, read b)
    _ -> Nothing

mulTuple :: (Int, Int) -> Int
mulTuple (a, b) = a * b

part1 :: String -> Int
part1 input = do
  let out = getAllTextMatches (input =~ "mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)") :: [String]
  let vals = map getNums out |> catMaybes |> map mulTuple
  trace (show out) $ sum vals

part2Inner :: Bool -> [String] -> [Maybe (Int, Int)]
part2Inner _ [] = []
part2Inner True (h : t) = case h of
  "do()" -> part2Inner True t
  "don't()" -> part2Inner False t
  _ -> getNums h : part2Inner True t
part2Inner False (h : t) = case h of
  "do()" -> part2Inner True t
  _ -> part2Inner False t

part2Process :: [String] -> [Int]
part2Process input = part2Inner True input |> catMaybes |> map mulTuple

part2 :: String -> Int
part2 input = do
  let out = getAllTextMatches (input =~ "((mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\))|(do\\(\\))|(don't\\(\\)))") :: [String]
  let vals = part2Process out
  trace (show out) $ sum vals

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  -- handle <- openFile "example.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
