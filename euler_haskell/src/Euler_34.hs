
module Euler_34 where

fact 0 = 1
fact n = n * fact (n-1)

factList = [fact x | x <- [1..9]]

threeDigits = [z | a <- [1..9], b <- [0..9], c <- [0..9], let z = 100*a+10*b+c, z == fact a + fact b + fact c]

fourDigits = [z | a <- [1..9], b <- [0..9], c <- [0..9], d <- [0..9], 
        let z = 1000*a+100*b+10*c+d, z == fact a + fact b + fact c + fact d]
        
fiveDigits = [z | a <- [1..9], b <- [0..9], c <- [0..9], d <- [0..9],  e <- [0..9],
        let z = 10000*a+1000*b+100*c+10*d+e, z == fact a + fact b + fact c + fact d + fact e] 
        
sixDigits = [z | a <- [1..9], b <- [0..9], c <- [0..9], d <- [0..9],  e <- [0..9], f <- [0..9],
        let z = 100000*a+10000*b+1000*c+100*d+10*e+f, z == fact a + fact b + fact c + fact d + fact e + fact f]  
        
sevenDigits = [z | a <- [1..2], b <- [0..9], c <- [0..9], d <- [0..9],  e <- [0..9], f <- [0..9], g <- [0..9],
        let z = 1000000*a+100000*b+10000*c+1000*d+100*e+10*f+g, z <= 2540160, z == fact a + fact b + fact c + fact d + fact e + fact f + fact g]
        
euler_34 = sum (threeDigits ++ fiveDigits)                        