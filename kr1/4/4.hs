supermap :: [a] -> (a -> [b]) -> [b]
supermap [] _ = []
supermap (x:xs) func = (func x) ++ (supermap xs func)