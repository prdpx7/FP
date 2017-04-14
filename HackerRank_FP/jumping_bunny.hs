xlcm :: (Integral n) => [n] -> Int -> n
xlcm [] _ = 1
xlcm [x,y] _ = lcm x y
xlcm (x:xs) n = lcm x (xlcm xs n)
main = do
    n <- readLn
    line <- getLine
    let xs = map (\x -> (read x :: Int)) $ words line
    print $ xlcm xs n