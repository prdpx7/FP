import Control.Applicative
import Control.Monad
import System.IO
import Data.Function

factorial = foldl (\acc x -> acc*x) 1 
 
calc :: Double -> Int -> Double
calc x n = (x^n)/(fromIntegral (factorial [1..n]) )*1.0

solve :: Double -> Int -> Double
solve x 0 = 1
solve x n = (calc x n) + solve x (n-1)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double
        print $ solve x 9