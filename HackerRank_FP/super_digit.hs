digit_sum x = if x `mod` 9 == 0 then 9 else x `mod` 9
main = do
    line <- getLine
    let [nstr,kstr] = words line
    let n = foldl (\acc x -> acc + (read [x] :: Int)) 0 nstr
    let k = read kstr :: Int
    print $ digit_sum((k*digit_sum n))