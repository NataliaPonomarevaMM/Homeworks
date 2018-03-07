import Data.List

help_same :: Char -> Char -> Bool
help_same '{' '}' = True
help_same '(' ')' = True
help_same '[' ']' = True
help_same _ _ = False

help_check [] "" = True
help_check [] _ = False
help_check (x:xs) str
		| elem x "[{(" = help_check xs (x:str)
		| elem x "]})" = if (str /= "" && help_same (head str) x)then help_check xs (tail str) else False
		| otherwise    = help_check xs str

check_str:: [Char] -> Bool
check_str str = help_check str ""