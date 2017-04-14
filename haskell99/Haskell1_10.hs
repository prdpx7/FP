module HaskellTen
( myLast
, myButLast
, elementAt
, myLength
, myReverse
, isPalindrome
, flatten 
, compress
, pack
, encode
) where

myLast :: [a] -> a
myLast [] = error "empty list !!!"
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "not enough elements !!"
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs

elementAt ::(Integral i) => [a] -> i -> a
elementAt [] i = error "idx > length of list !!"
elementAt (x:xs) i 
        | i <= 0 = error "idx should be >= 1"
        | i == 1 = x
        | otherwise = elementAt xs (i-1)


myLength :: (Integral x) => [a] -> x
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) 
    | x == (head xs) = [] ++ compress xs
    | otherwise = [x] ++ compress xs

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) 
        | x `elem` (head(pack xs)) = (x:head (pack xs)): (tail(pack xs))
        | otherwise =  [x]:(pack xs)

encode ::(Eq a) => [a] -> [(Int,a)]
encode xs = let ys = pack xs in map (\x -> (length x, head x)) ys

