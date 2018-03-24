find_num :: [Int] -> Int -> Int
find_num list n = if newlist == [] then (-1) else snd $ head $ newlist
	where newlist = filter (\x -> fst x == n) (zip list [1..])
