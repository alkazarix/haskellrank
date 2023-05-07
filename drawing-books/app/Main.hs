module Main (main) where


solve :: [Int] -> Int
solve [n,p] = min fromFront fromBack 
    where 
        fromFront = p `div` 2
        fromBack = n `div` 2 - fromFront

main :: IO ()
main = interact $  show . solve .  map read . words
