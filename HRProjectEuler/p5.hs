---- https://www.hackerrank.com/contests/projecteuler/challenges/euler005
import Control.Applicative
import Control.Monad
import System.IO

xlcm :: Int -> Int
xlcm 1 =  1
xlcm n = lcm n (xlcm (n-1))

main :: IO ()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    forM_ [1..t] $ \a0  -> do
        n_temp <- getLine
        let n = read n_temp :: Int
        print $ xlcm n
