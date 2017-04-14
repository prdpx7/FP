module Haskell20
( encodeModified,
  decodeModified,
  encodeDirect,
  dupli,
  dropEvery,
  split,
  slice
) where

group' [] = []
group' (x:xs) = (x:(filter (==x) xs)):(group' $ filter (/=x) xs)

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified xs = [ ans (head x)|x <- group' xs, let ans = if (length x) == 1 then Single else Multiple (length x)]

decodeModified :: [ListItem a] -> [a]
decodeModified xs = foldl(\acc x -> acc ++ (decodeTuple x))[] xs
    where
        decodeTuple (Single x) = [x]
        decodeTuple (Multiple n x) = replicate n x



encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = [encodeTuple pattern] ++ encodeDirect remaining
        where 
            (pattern,remaining) = span (==x) (x:xs)
            encodeTuple xs
                | length xs > 1 = (Multiple (length xs) (head xs))
                | otherwise = (Single (head xs))


dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs


repli :: [a] -> Int -> [a]
repli xs n = foldl (\acc x -> acc ++ repliHelp x n) [] xs
    where 
        repliHelp _ 0 = []
        repliHelp x n = x : repliHelp x (n-1)

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropHelp xs 1 n 
    where
        dropHelp [] _ _ = []
        dropHelp (x:xs) i n 
            | i `mod` n == 0 = dropHelp xs (i+1) n
            | otherwise = x:dropHelp xs (i+1) n

split :: (Eq a) => [a] -> Int -> ([a],[a])
split xs n = ((splitL xs n),(splitR xs 1 n))
    where 
        splitL [] _ = []
        splitL _ 0 = []
        splitL (x:xs) n = [x] ++ splitL xs (n-1)

        splitR (x:xs) i n
                | n < 0 = []
                | xs == [] = []
                | i > n = (x:xs)
                | i == n = xs
                | otherwise = splitR xs (i+1) n 


slice :: (Eq a) => [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs l r = sliceHelp 1 l r xs
    where
        sliceHelp i l r (x:xs) 
            | i < l = sliceHelp (i+1) l r xs
            | i < r = [x] ++ sliceHelp (i+1) l r xs
            | i == r = [x]
            | i > r = []
