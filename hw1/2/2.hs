suchelp :: Integer -> Integer -> Integer -> Integer
suchelp prev cur 2 = prev + cur            
suchelp prev cur n = suchelp cur (prev + cur) (n - 1)

subhelp :: Integer -> Integer -> Integer -> Integer
subhelp prev cur (-1) = cur - prev          
subhelp prev cur n = subhelp (cur - prev) prev (n + 1)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 0 = suchelp 0 1 n
            | n < 0 = subhelp 0 1 n