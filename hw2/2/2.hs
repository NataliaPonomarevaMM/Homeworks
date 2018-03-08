help_power :: [Int] -> Int -> [Int]
help_power list 0 = list
help_power list cur = help_power (2 ^ cur : list) (cur - 1)

powers_of_2 :: Int -> [Int]
powers_of_2 n = help_power [] n