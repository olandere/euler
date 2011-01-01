
module Euler_38 where

pandigital :: Integer -> [Integer]
pandigital n = 
        pandigital' n (n-1) []

pandigital' :: Integer -> Integer -> [Integer] -> [Integer]         
pandigital' n r l 
     | r == 0 = [x | x <- [1..n], x `notElem` l]
     | otherwise = concat$[y | a <- [1..n], a `notElem` l, let y = map ((+) (a * 10 ^ r)) (pandigital' n (r-1) (a:l)) ] 

filter2 n =
        let a = n `div` 10^5
            b = n `rem` 10^5
        in a * 2 == b
        
filter3 n = 
        let a = n `div` 10 ^ 6
            b = (n `div` 10 ^ 3) `rem` 10^3
            c = n `rem` 10^3
        in a * 2 == b && a * 3 == c 
        
filter4 n = 
        let a = n `div` 10 ^ 7
            b = (n `div` 10 ^ 5) `rem` 10^2
            c = (n `div` 10 ^ 3) `rem` 10^2
            d = n `rem` 10^3
        in a * 2 == b && a * 3 == c && a * 4 == d 
        
filter5 n = 
        let a = n `div` 10 ^ 8
            b = (n `div` 10 ^ 6) `rem` 10^2
            c = (n `div` 10 ^ 4) `rem` 10^2
            d = (n `div` 10 ^ 2) `rem` 10^2
            e = n `rem` 10^2
        in a * 2 == b && a * 3 == c && a * 4 == d && a * 5 == e          
        
filter23 n = filter2 n || filter3 n || filter4 n || filter5 n
        
euler38 = maximum$filter filter23 (pandigital 9)               