sum_of_nums :: Integer -> Integer
sum_of_nums 0 = 0
sum_of_nums x | x < 0 = sum_of_nums $ x * (-1)
                                    | otherwise =  sum_of_nums (div x 10) + mod x 10