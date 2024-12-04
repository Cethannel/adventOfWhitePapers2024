module Main where

import Data.List (transpose)
import Data.Matrix (Matrix, fromLists, mapPos, ncols, nrows, submatrix, toList, toLists, zero)
import Data.Maybe (catMaybes)
import Data.Universe.Helpers (diagonals)
import Debug.Trace (trace)
import System.IO
import Text.Regex.Base
import Text.Regex.TDFA
import Text.Regex.TDFA.Common (flipOrder)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

getXmas :: String -> [String]
getXmas line = getAllTextMatches (line =~ "XMAS")

convert :: [String] -> Int
convert input = trace (show input) $ map getXmas input |> map length |> sum

part1 :: String -> Int
part1 input = do
  let inputLines = lines input
  let initial = lines input
  let flipped = map reverse inputLines
  let trans = transpose inputLines
  let transFlipped = transpose inputLines |> map reverse
  let diags1 = diagonals initial
  let diags2 = diagonals flipped
  let diags3 = reverse initial |> diagonals
  let diags4 = reverse flipped |> diagonals
  trace (show flipped) $ map convert [initial, flipped, trans, transFlipped, diags1, diags2, diags3, diags4] |> sum

isXmas :: [[Char]] -> Bool
isXmas [['M', _, 'M'], [_, 'A', _], ['S', _, 'S']] = True
isXmas [['M', _, 'S'], [_, 'A', _], ['M', _, 'S']] = True
isXmas [['S', _, 'M'], [_, 'A', _], ['S', _, 'M']] = True
isXmas [['S', _, 'S'], [_, 'A', _], ['M', _, 'M']] = True
isXmas input = trace (show input) False

getSub :: Matrix Char -> (Int, Int) -> Matrix Char
getSub input (x, y) = do
  let ex = x + 2
  let ey = y + 2
  let subMat = submatrix x ex y ey input
  trace (show subMat) subMat

part2 :: String -> Int
part2 input = do
  let inputLines = lines input
  let mat = fromLists inputLines
  let rowNums = nrows mat
  let colsNums = ncols mat
  let zeroMat = zero (rowNums - 2) (colsNums - 2) :: Matrix Int
  let positions = mapPos const zeroMat |> toList
  trace (show mat) $ map (getSub mat) positions |> map toLists |> filter isXmas |> length

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  -- handle <- openFile "example.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
