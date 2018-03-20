list_1 :: [Int]
list_1 = 1 : (map (* (-1)) list_1)

list_1_2 :: [Int]
list_1_2 = zipWith (*) [1..] list_1