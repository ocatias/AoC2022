import Data.List.Split
import Data.List

get_marker_position distinct_char pos string
    | (length $ nub (take distinct_char string)) == distinct_char = pos
    | otherwise = get_marker_position distinct_char (pos + 1) (drop 1 string)

main = do
    rawInput <- readFile ("input.txt")
    let line = (init $ splitOn "\n" rawInput)!!0
    print $ get_marker_position 4 4 line
    print $ get_marker_position 14 14 line