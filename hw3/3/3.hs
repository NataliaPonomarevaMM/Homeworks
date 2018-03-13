help_find :: (Num a, Ord a )=> [a] -> Int -> a -> Int -> Int
help_find [] pos _ _  = pos
help_find (x : []) pos _ _ = pos
help_find (x1 : x2 : xs) pos sum cur = if (x1 + x2 > sum) then help_find (x2 : xs) cur (x1 + x2) (cur + 1)
												else  help_find (x2 : xs) pos sum (cur + 1)

find_max :: (Num a, Ord a )=> [a] -> Int
find_max list = help_find list (-1) 0 1
