import Data.Map
import Data.Char
data PhoneBook = PB (Map String String) deriving Show

add :: PhoneBook -> (String, String) -> PhoneBook
add (PB map) (name, tel) =  PB (insert name tel map)

searchByPhone :: PhoneBook -> String -> Maybe String
searchByPhone (PB map) phone = foldrWithKey (\key val b ->  if val == phone then (Just key) else Nothing) Nothing map

searchByName :: PhoneBook -> String -> Maybe String
searchByName (PB map) name = Data.Map.lookup name map

exportPB :: PhoneBook -> String
exportPB (PB map) = concatMap (\(key,val) -> key ++ " " ++ val ++ "\n") (toList map)

helpfunc [a,b] = (a,b)
helpfunc list = error "error in input data"

importPB :: String -> PhoneBook
importPB str = (Prelude.foldl add (PB empty)) (Prelude.map (helpfunc . words) (lines str))

doMain :: PhoneBook -> IO()
doMain pb = do
    putStrLn "Input 0-5"
    number <- getChar
    r <- getLine
    case number of 
        '0' -> return ()
        '1' -> do 
			putStrLn "Input name:"
			name <- getLine
			putStrLn "Input number:"
			tel <- getLine
			if ((all isLetter name) && (all isDigit tel)) 
			then doMain (add pb (name, tel))
			else return ()
        '2' -> do
			putStrLn "Input name:"
			name <- getLine
			putStrLn $ show (searchByName pb name)
			doMain pb
        '3' -> do
			putStrLn "Input number:"
			tel <- getLine
			putStrLn $ show(searchByPhone pb tel)
			doMain pb
        '4' -> do
			putStrLn "Input file name:"
			filename <- getLine
			writeFile filename (exportPB pb)
			doMain pb
        '5' -> do
			putStrLn "Input file name:"
			filename <- getLine
			pbContents <- readFile filename
			doMain $ importPB pbContents
        otherwise -> do
				doMain pb

main = do doMain (PB Data.Map.empty)