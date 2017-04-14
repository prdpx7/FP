removeEle :: [Char] -> Char -> [Char]
removeEle [] _ = []
removeEle (x:xs) a 
    | a == x = [] ++ (removeEle xs a)
    | otherwise = [x] ++ (removeEle xs a)
removeDup :: [Char] -> [Char]
removeDup [] = []
removeDup (x:xs) = [x] ++ (removeDup (removeEle xs x))
main = do
    line <- getLine
    putStrLn $ removeDup line