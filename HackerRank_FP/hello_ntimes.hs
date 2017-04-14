hello_worlds n = let ans = foldl (\x acc -> acc ++ x) ""  $ take n $ repeat "Hello World\n" in putStr ans
main = do
   n <- readLn :: IO Int
   hello_worlds n
