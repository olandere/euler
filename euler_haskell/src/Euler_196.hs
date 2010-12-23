
module Euler_196 where

import Array

sumUpTo :: Integral a => a -> a
sumUpTo n = div (n * (n + 1)) 2

-- rowBounds :: Integer -> (Integer, Integer)
rowBounds n = 
        let last = sumUpTo n
        in (last - n + 1, last)

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
 
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters
   
primes    = 2: oddprimes
oddprimes = 3: sieve oddprimes 3 0
sieve :: [Integer] -> Integer -> Int -> [Integer]
sieve (p:ps) x k 
          = [n | n <- [x+2,x+4..p*p-2]
                 , and [rem n p/=0 | p <- take k oddprimes]]
            ++ sieve ps (p*p) (k+1)
            
---primesFrom m   = sieve (length h) ps $ m`div`2*2+1 
--    where 
--      (h,(_:ps)) = span (<= (floor.sqrt.fromIntegral) m) primes
      
      
-- need primes up to squareRoot.fst$(rowBounds n)

-- takeWhile (<=(snd r)) (dropWhile (<(fst r)) primes)

primesFromTo a b = (if a<3 then [2] else []) 
                     ++ [i | i <- [o,o+2..b], ar ! i]
   where 
    o  = max (if even a then a+1 else a) 3
    r  = squareRoot $ b+1
    ar = accumArray (\a b-> False) True (o,b) 
          [(i,()) | p <- [3,5..r]
                    , let q  = p*p 
                          s  = 2*p 
                          (n,x) = quotRem (o - q) s 
                          q' = if  o <= q  then q
                               else  q + (n + signum x)*s
                    , i <- [q',q'+s..b] ]
                    
isPrime n = n > 1 && n == head (primeFactors n)
 
primeFactors 1 = []
primeFactors n = go n primes
     where
     go n ps@(p:pt)
        | p*p > n        = [n]
        | n `rem` p == 0 = p : go (n `quot` p) ps
        | otherwise      = go n pt      

inBounds x n =
        let (f, l) = rowBounds n
        in x >= f && x <= l

test :: Integer -> Integer -> [(Integer, Integer)]
test a b = if (inBounds a b && isPrime a) then [(a,b)] else []

testAbove :: Integer -> Integer -> [(Integer, Integer)]
testAbove x n = test x n ++ test (x+1) n ++ test (x+2) n

testBelow :: Integer -> Integer -> [(Integer, Integer)]
testBelow x n = test (x-1) n ++ test x n ++ test (x+1) n

primeTriplet x n = 
        let above = testAbove (x-n) (n-1)
            below = testBelow (x+n) (n+1)
        in if length (above++below) >= 2 then True else 
              if (length above) > 0 && (length$testAbove ((fst (head above))-(snd (head above))) ((snd$head above)-1)) > 0 then True else
              if (length below) > 0 && (length$testBelow ((fst (head below))+(snd (head below))) ((snd$head below)+1)) > 0 then True else False
                 
               
--      sum [test (x-n) (n-1),test (x-n+1) (n-1),test (x-n+2) (n-1),
--           test (x+n-1) (n+1),test (x+n) (n+1),test (x+n+1) (n+1)] >= 2
        
                
funcS n =
        let (f, l) = rowBounds n
        in sum [x | x <- (primesFromTo f l), primeTriplet x n]     
        
euler_196 = (funcS 5678027) + (funcS 7208785)