list_of_179 :: [Int]
list_of_179 = 1 : 7 : 9 : concatMap (add_179) (map (* 10) list_of_179)
	where add_179 x = [x + 1, x + 7, x + 9]