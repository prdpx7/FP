import Data.List
xcompress :: (Eq a) => [a] -> [(Int,a)]
xcompress xs =  map (\x -> (length x,head x))  $ group xs

strcompression ::[(Int,Char)] -> String
strcompression [] = ""
strcompression ((num,x):xs)
    | num == 1 = [x] ++ strcompression xs
    | otherwise = [x] ++ (show num) ++ strcompression xs
main = do
    line <- getLine
    let tmp = xcompress line
    putStrLn $ strcompression tmp