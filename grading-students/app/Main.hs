module Main (main) where

rang5 :: Int -> Int 
rang5 x 
    | x >= 38 && (mutpl5 - x) < 3 = mutpl5
    | otherwise = x
    where
        mutpl5 = x + (5 - x `mod` 5)

solve :: [Int] -> [Int]
solve = map rang5

main :: IO ()
main = interact $ unlines .  map show . solve . map read . tail . words 
