import Control.Applicative
import Control.Monad
import System.IO

check :: Int -> Int -> Int -> Int -> Bool
check r g y b 
    | abs(r-g) > 1 = False
    | abs (y-b) > 1 = False
    | otherwise = True
solve :: [Char] -> Int -> Int -> Int -> Int -> Bool
solve [] r g y b 
    | (r == g) && (y == b) = True
    | otherwise = False
solve (x:xs) r g y b
    | x == 'R' && check r g y b = solve xs (r+1) g y b
    | x == 'G' && check r g y b = solve xs r (g+1) y b
    | x == 'Y' && check r g y b = solve xs r g (y+1) b
    | x == 'B' && check r g y b = solve xs r g y (b+1)
    | otherwise = False
    
main :: IO () 
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        patstr <- getLine
        print $ solve patstr 0 0 0 0
