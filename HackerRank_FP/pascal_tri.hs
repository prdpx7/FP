pascal :: Int -> Int -> Int
pascal n r 
    | r > n = 0
    | r == 0 = 1
    | n == r = 1
    | otherwise = pascal (n-1) (r-1) + pascal (n-1) r

main = do
    k <- readLn
    let rows m = map (pascal m) [0..m]
    let ans = map rows [0..(k-1)] 
    putStr $ unlines $ map unwords $ map (\row -> (map (\x -> show x) row)) ans