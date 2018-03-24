module Main where 
import  System.IO

main :: IO ()			
main = do
   mainLoop []

addValue :: [Int] -> Int -> [Int]
addValue [] y = [y]
addValue (x:xs) y | x < y = x : (addValue xs y)
				| otherwise = y:x:xs

removeValue :: [Int] -> Int -> [Int]
removeValue [] _ = []
removeValue (x:xs) y | x < y  = x:(removeValue xs y)
					| x == y = removeValue xs y
					| x > y  = x:xs

mainLoop :: [Int] -> IO ()
mainLoop list = do
    putStrLn "0 - exit"
    putStrLn "1 - add value to sorted list"
    putStrLn "2 - remove value from list"
    putStrLn "3 - print list"
    input <- getLine
    case head input of
        '0' -> return ()
        '1' -> do 
		putStrLn "Enter value to add: "
		value <- getLine
		mainLoop (addValue list $ read value)
        '2' -> do 
		putStrLn "Enter value to remove: "
		value <- getLine
		mainLoop (removeValue list $ read value)
        '3' -> do
		print list
		mainLoop list
        otherwise -> do
		putStrLn "Error, please try again"
		mainLoop list
                               
