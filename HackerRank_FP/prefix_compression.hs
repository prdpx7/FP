patternMatch :: (Eq a) => [a] -> [a] -> [a]
patternMatch [] _ = []
patternMatch _ [] = []
patternMatch (x:xs) (y:ys)
    | x == y = [x] ++ patternMatch xs ys
    | otherwise = []
extractRest :: (Eq a) => [a] -> [a] -> [a]
extractRest (x:xs) (y:ys)
    | ys == [] = xs
    | otherwise = extractRest xs ys
main = do
    xarr <- getLine
    yarr <- getLine
    let matched = patternMatch xarr yarr
    let xrest =  extractRest xarr matched
    let yrest =  extractRest yarr matched
    let ans = [matched,xrest,yrest]
    putStr $ unlines $ map (\x -> (show (length x) ++ " " ++ x)) ans