import Control.Monad
import Data.Function ((&))
import Data.List
import Numeric
import System.IO

getPairAsInt = map (map (\x -> (read x :: Int)))

getPairsList = map words . lines

difference x y = abs (x - y)

tuplify2 [x, y] = (x, y)

sortTuple (x, y) = (sort x, sort y)

getTwoListsAsTuples x =
  x
    & getPairsList
    & getPairAsInt
    & map tuplify2
    & unzip

count x = length . filter (x ==)

getCounts :: ([Int], [Int]) -> [(Int, Int)]
getCounts tuple = map (\x -> (x, count x (snd tuple))) (fst tuple)

main =
  do
    contents <- readFile "day_1_data.txt"

    contents
      & getTwoListsAsTuples
      -- get counts paired with the element
      & getCounts
      -- weighted reduce
      & map (\(x, count) -> x * count)
      & sum
      & print
