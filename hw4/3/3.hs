import Data.Foldable

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Foldable Tree where
   foldMap f Empty = mempty
   foldMap f (Leaf x) = f x
   foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
   
getLeafs :: Tree a -> [a]
getLeafs = toList

--another version
getLeafs' :: Tree a -> [a]
getLeafs' tree = foldMap (:[]) tree