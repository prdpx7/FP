strRotate :: String -> Int -> String
strRotate xs 0 = ""
strRotate (x:xs) n = xs ++ [x] ++ " " ++ strRotate (xs ++ [x]) (n-1)

runtest :: Int -> IO()
runtest 0 = return()
runtest n = do
    line <- getLine
    putStrLn $ strRotate line (length line)
    runtest (n-1)

main = do
    n <- readLn
    runtest n