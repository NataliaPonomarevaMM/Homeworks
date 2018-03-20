check_list :: (a -> Bool) -> [a] -> Bool
check_list _ [] = True
check_list func (x:xs) | func x == False = False
					   | otherwise = check_list func xs