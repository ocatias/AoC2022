import Data.List.Split

do_move transpose stacks amount from to = left ++ [newLeft] ++ center ++ [newRight] ++ right
    where   left = if (lower == 0) then [] else [stacks!!i | i <- [0..lower-1]] 
            center = if (lower == upper) then [] else [stacks!!i | i <- [lower+1..upper-1]]
            right = if (upper == (length stacks)-1) then [] else [stacks!!i | i <- [upper+1..(length stacks)-1]]
            lower = minimum [from, to]
            upper = maximum [from, to]
            amountSafe = minimum [length (stacks!!from), amount]
            stackToMove = transpose $ take amountSafe (stacks!!from)
            oldStackTo = if (from /= to) then (stacks!!to) else (drop amountSafe (stacks!!to))
            newStackFrom = if (from /= to) then  drop amountSafe (stacks!!from) else []
            newStackTo = stackToMove ++ oldStackTo
            newLeft = if (from < to) then newStackFrom else newStackTo
            newRight = if (from >= to) then newStackFrom else newStackTo

rec_move transpose (x:xs) stacks = rec_move transpose xs (do_move transpose stacks (x!!0) (x!!1 - 1) (x!!2 - 1))
rec_move _ [] stacks = stacks

main = do
    rawInput <- readFile ("input.txt")
    let rows = reverse $ drop 1 $ reverse $ splitOn "\n" $ (splitOn "\n\n" rawInput)!!0
    let movesString =  map (splitOn " ")  $ splitOn "\n" $ (splitOn "\n\n" rawInput)!!1
    let moves = map (map (read::[Char]->Int)) $ map (\ls -> [ls!!1, ls!!3 , ls!!5]) $ take ((length movesString) - 1) movesString
    let state = [[(rows!!j)!!(1+i*4) | j <- [0..7], ((rows!!j)!!(1+i*4)) /= ' ' && ((rows!!j)!!(1+i*4)) /= '-'] | i <- [0..8]]
    print $ map (\ls -> ls!!0) (rec_move reverse moves state) 
    print $ map (\ls -> ls!!0) (rec_move (\x -> x) moves state) 
