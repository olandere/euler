
module Euler_52 where

import List

sameDigits :: Integer -> Integer -> Bool
sameDigits m n =
        (sort$show n) == (sort$show m)
        
sameDigitsFilter n =
       sameDigits (n*2) (n*3) &&
       sameDigits (n*3) (n*4) &&
       sameDigits (n*4) (n*5) &&
       sameDigits (n*5) (n*6)  

euler52 = head [x | x <- [1..], sameDigitsFilter x]