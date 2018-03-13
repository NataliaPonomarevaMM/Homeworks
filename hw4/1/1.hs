countEven1 :: (Integral a) => [a] -> Int
countEven1 list = foldr (\x y -> if mod x 2 == 0 then y + 1 else y) 0 list

countEven2 :: (Integral a) => [a] -> Int
countEven2 list = foldr (const (1 +)) 0 (filter even list)

countEven3::  [Int] -> Int
countEven3 list = foldr (+) 0 (map (flip mod 2) list)