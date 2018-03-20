matrix :: Int -> [[Int]]
matrix n = map (\x -> map (\y -> if y < x then x else y) (take n [1..])) (take n [1..])