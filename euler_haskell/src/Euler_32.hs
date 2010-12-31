
module Euler_32 where

import qualified Data.Set as S
nub' :: (Ord a) => [a] -> [a]
nub' = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs
                    
pandigital :: Integer -> [Integer]
pandigital n = 
        pandigital' n (n-1) []

pandigital' :: Integer -> Integer -> [Integer] -> [Integer]         
pandigital' n r l 
     | r == 0 = [x | x <- [1..n], x `notElem` l]
     | otherwise = concat$[y | a <- [1..n], a `notElem` l, let y = map ((+) (a * 10 ^ r)) (pandigital' n (r-1) (a:l)) ] 
     
property n =
     let a = n `div` 10^7
         b = (n `div` 10^4) `rem` 10^3
         a' = n `div` 10^8
         b' = (n `div` 10^4) `rem` 10^4
         c = n `rem` 10^4
     in a * b == c || a' * b' == c
     
frem = flip$rem
     
euler32 = sum.nub'$map (frem (10^4)) (filter property (pandigital 9) ) 