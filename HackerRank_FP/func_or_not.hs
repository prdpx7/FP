import Control.Monad
import Data.List

valid :: [(Int, Int)] -> Bool
valid [] = True
valid [x] = True
valid (x:y:ys)
    | (fst x == fst y,snd x /= snd y) == (True,True) = False
    | otherwise = valid (y:ys)

main = do
    t <- fmap (read::String->Int) getLine
    forM [1..t] (\_->do
        n <- fmap (read::String->Int) getLine
        func <- forM [1..n] (\_->do fmap ((\[a, b]->(a,b)).map (read::String->Int).words) getLine :: IO (Int, Int))
        putStrLn $ if valid $ sort func then "YES" else "NO")