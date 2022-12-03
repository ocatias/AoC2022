import Data.List.Split
import Data.List
import Data.Char

get_value x
  | ord x < 97 = ord x - 65 + 27
  | otherwise = ord x - 97 + 1

get_duplicates ls = nub $ intersect (take len ls) (drop len ls)
  where len = (length ls) `div` 2

generate_triplet [] = []
generate_triplet (x:y:z:ls) = [x,y,z]:(generate_triplet ls)

main = do
    rawInput <-  readFile ("input.txt")
    let backpacks = init (splitOn "\n" rawInput)
    print $ sum $ map sum $ map (map get_value) $ map get_duplicates backpacks
    let triplets = generate_triplet backpacks
    print $ sum $ map (\(x:xs) -> get_value x) $ map nub $ map (\ls -> foldr1 intersect ls) triplets
