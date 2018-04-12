help :: Int -> Int -> [IO ()] -> IO ()
help n k  list | n == 1 && k == 1 = showseq
			| k == 1                    = withk
			| n == k                    =  showseq >> withoutk
			| n < 0 || k < 0      = return ()
			| otherwise         = withk >> withoutk
		where 
			showseq = sequence_ ( reverse ((putStrLn $ show k) : list))
			withk = help (n - k) k ((putStr $ (show k) ++ " + ") : list)
			withoutk = help n (k - 1) list

terms :: Int -> IO ()
terms n = help n n [putStr $ (show n) ++ " = "]