import Data.List.Split
import Data.List

main = do 
    rawInput <-  readFile ("input.txt")
    let elves_calories_ls = map (map ((\x -> read x)::(String -> Int)))  $ splitOn [""] (init (splitOn "\n" rawInput))
    let elves_total_calories = map sum elves_calories_ls
    print (maximum elves_total_calories)
    print $ sum $ take 3 (sortBy (flip compare) elves_total_calories)