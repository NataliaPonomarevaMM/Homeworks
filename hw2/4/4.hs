help_find :: [Int] -> Int -> Int -> Int
help_find [] _ _ = -1
help_find (x:xs) cur n | x == n = cur
                                                           | otherwise = help_find xs (cur + 1) n

find_num :: [Int] -> Int -> Int
find_num list n = help_find list 0 n