import Data.List.Split

get_pts_shape "X" = 1
get_pts_shape "Y" = 2
get_pts_shape "Z" = 3

map_to_xyz "A" = "X"
map_to_xyz "B" = "Y"
map_to_xyz "C" = "Z"

get_pts_winner "X" "Y" = 6
get_pts_winner "Y" "Z" = 6
get_pts_winner "Z" "X" = 6
get_pts_winner x y 
    | x == y = 3
    | otherwise = 0

shape_to_play x "Y" = x
shape_to_play "X" "X" = "Z"
shape_to_play "Y" "X" = "X"
shape_to_play "Z" "X" = "Y"
shape_to_play "X" "Z" = "Y"
shape_to_play "Y" "Z" = "Z"
shape_to_play "Z" "Z" = "X"

get_pts_winner2 "X" = 0
get_pts_winner2 "Y" = 3
get_pts_winner2 "Z" = 6

main = do 
    rawInput <-  readFile ("input.txt")
    let moves = map (\x -> splitOn [' '] x) (init (splitOn "\n" rawInput))
    print $ sum $ map (\ls -> (get_pts_winner (map_to_xyz (ls!!0)) (ls!!1)) + (get_pts_shape (ls!!1))) moves    
    print $ sum $ map (\ls -> (get_pts_winner2 (ls!!1)) + (get_pts_shape (shape_to_play (map_to_xyz (ls!!0)) (ls!!1)))) moves