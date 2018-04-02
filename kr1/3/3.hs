makeRombus n = do putStr (count_str n)

make ::Char -> Int -> [Char]
make c i = map (\x -> c) (take i [1..])

count_str n =  concatMap (\x -> (make ' ' (n - x)) ++ (make 'x' (2 * x - 1)) ++(make ' ' (n - x))++"\n" ) linen
		where 
		linen = (taken ++ [n] ++ (reverse taken))
		taken = take (n - 1) [1..]