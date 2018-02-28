help_reverse :: [a] -> [a] -> [a]
help_reverse [] list = list
help_reverse (x:xs) list = help_reverse xs (x : list)

reverse :: [a] -> [a]
reverse l = help_recurse l []