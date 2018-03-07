help_fib :: Int -> Int -> Int -> Int
help_fib curr prev n | n == 0 = curr
                     | n > 0  = help_fib (curr + prev) curr ( n - 1)
		     | n < 0  = help_fib prev (curr - prev) ( n + 1)

fib :: Int -> Int
fib n = help_fib 0 1 n
