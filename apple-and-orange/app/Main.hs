{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main (main) where


solve :: [Int] -> [Int]
solve (s:t:a:b:m:_:rest) = [length apples, length oranges]
    where
        apples = filter (\x -> s <=x && x <= t) 
                $ map (\x -> x + a) 
                $ take m rest
        oranges = filter (\x -> s <=x && x <= t) 
            $ map (\x -> x + b) 
            $  drop m rest

main :: IO ()
main = interact $ unlines . map show . solve . map read. words
