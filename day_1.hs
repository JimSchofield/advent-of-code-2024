import System.IO  
import Control.Monad
import Numeric
import Data.Function ((&))
import Data.List


getPairAsInt = map (map (\x -> (read x :: Int)))

getPairsList = map words . lines

difference x y = abs(x - y)

tuplify2 [x,y] = (x,y)

sortTuple (x,y) = (sort x, sort y)


main = do  
        contents <- readFile "day_1_data.txt"

        contents
          & getPairsList
          & getPairAsInt
          & map tuplify2
          & unzip
          & sortTuple
          & \(x,y) -> zipWith difference x y
          & sum
          & print
