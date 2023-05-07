{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
module Main (main) where

import Data.List

delta :: Char -> Int
delta 'U' = 1
delta 'V' = -1
delta  _  = error "invalid input."

solve :: String -> Int
solve  s = length  
            $ filter (all (<0)) 
            $ groupBy (\x y -> x /= 0 && y /= 0)$ scanl (+) 0 
            $ map delta s

main :: IO ()
main = interact $ show . solve .  head . tail . words 
