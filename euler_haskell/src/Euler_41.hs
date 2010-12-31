
module Euler_41 where

isPrime n = n > 1 && n == head (primeFactors n)
 
primes    = 2: oddprimes
oddprimes = 3: sieve oddprimes 3 0
sieve :: [Integer] -> Integer -> Int -> [Integer]
sieve (p:ps) x k 
          = [n | n <- [x+2,x+4..p*p-2]
                 , and [rem n p/=0 | p <- take k oddprimes]]
            ++ sieve ps (p*p) (k+1)
            
primeFactors 1 = []
primeFactors n = go n primes
     where
     go n ps@(p:pt)
        | p*p > n        = [n]
        | n `rem` p == 0 = p : go (n `quot` p) ps
        | otherwise      = go n pt      

primeLastDigit = [1,3,7,9]

pandigital :: Integer -> [Integer]
pandigital n = 
        pandigital' n (n-1) []

pandigital' :: Integer -> Integer -> [Integer] -> [Integer]         
pandigital' n r l 
     | r == 0 = [x | x <- [1..n], x `notElem` l]
     | otherwise = concat$[y | a <- [1..n], a `notElem` l, let y = map ((+) (a * 10 ^ r)) (pandigital' n (r-1) (a:l)) ] 
     
euler41 n = maximum$filter isPrime (pandigital n) 
           
answer = euler41 7