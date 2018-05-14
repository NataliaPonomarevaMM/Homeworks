import Control.Monad

--fist and last elements are not taken
find_first [] = Nothing
find_first [a] = Nothing
find_first [a,b] = Nothing
find_first (a:b:c:xs) = mplus (if ((a < b) &&(c < b)) then (Just b) else Nothing) (find_first (b:c:xs))