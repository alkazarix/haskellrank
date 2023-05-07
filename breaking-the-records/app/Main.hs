{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
module Main (main) where

import Data.List


solve :: [Int] -> [Int]
solve  xs= [records maximum xs, records minimum xs]
    where 
        records :: ([Int] -> Int) -> [Int] -> Int
        records f ys = (length $ group $ map f $ tail $ inits ys)  -1 

main :: IO ()
main = interact $ unwords .  map show . solve . map read. tail. words 
