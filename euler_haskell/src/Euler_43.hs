
module Euler_43 where

pandigital :: Integer -> [Integer]
pandigital n = 
        pandigital' n n []

pandigital' :: Integer -> Integer -> [Integer] -> [Integer]         
pandigital' n r l 
     | r == 0 = [x | x <- [0..n], x `notElem` l]
     | r == n = concat$[y | a <- [1..n], a `notElem` l, let y = map ((+) (a * 10 ^ r)) (pandigital' n (r-1) (a:l)) ]
     | r == n - 3 = concat$[y | a <- [0,2..8], a `notElem` l, let y = map ((+) (a * 10 ^ r)) (pandigital' n (r-1) (a:l)) ]
     | r == n - 5 = concat$[y | a <- [0,5], a `notElem` l, let y = map ((+) (a * 10 ^ r)) (pandigital' n (r-1) (a:l)) ]
     | otherwise = concat$[y | a <- [0..n], a `notElem` l, let y = map ((+) (a * 10 ^ r)) (pandigital' n (r-1) (a:l)) ] 
     
property n =
     let a = ((n `div` 10^5) `rem` 10^3) `rem` 3 == 0
         b = ((n `div` 10^3) `rem` 10^3) `rem` 7 == 0
         c = ((n `div` 10^2) `rem` 10^3) `rem` 11 == 0
         d = ((n `div` 10) `rem` 10^3) `rem` 13 == 0
         e = (n `rem` 10^3) `rem` 17 == 0
     in a && b && c && d && e 
     
euler43 = sum$filter property (pandigital 9)
   