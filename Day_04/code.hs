import Data.List.Split

is_subset [[x1,x2],[y1,y2]] = ((x1 >= y1) && (x2 <= y2)) || ((x1 <= y1) && (x2 >= y2))
is_overlap [[x1,x2],[y1,y2]] = not ((x2 < y1) || (x1 > y2))

main = do
    rawInput <- readFile ("input.txt")
    let pairs = map (map (map (read::String->Int))) $ map (map (splitOn "-")) $ map (splitOn ",") $ init (splitOn "\n" rawInput)
    print $ length $ filter is_subset pairs
    print $ length $ filter is_overlap pairs
