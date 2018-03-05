first :: [Int] -> Int
first [] = 0
first a = head a

ending :: [Int] -> [Int]
ending [] = []
ending a = tail a

sum_lists :: [Int] -> [Int] -> [Int] -> [Int]
sum_lists [] [] [] = []
sum_lists a b c = (first a + first b + first c) : sum_lists (ending a)  (ending  b)  (ending c)