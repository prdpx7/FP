---- https://www.hackerrank.com/contests/projecteuler/challenges/euler001
import Control.Applicative
import Control.Monad
import System.IO

nsum :: Int -> Int -> Int -> Int
nsum n a an = (n*(a + an)) `div` 2

solve :: Int -> Int
solve x = (nsum n1 3 (n1*3)) + (nsum n2 5 (n2*5)) - (nsum n3 15 (n3*15))
    where n1 = x `div` 3
          n2 = x `div` 5
          n3 = x `div` 15
main :: IO ()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    forM_ [1..t] $ \a0  -> do
        n_temp <- getLine
        let n = read n_temp :: Int
        print $ solve (n-1)
