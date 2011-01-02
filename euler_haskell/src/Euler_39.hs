
module Euler_39 where
import List

primPythag u v =
        let u2 = u * u
            v2 = v * v
        in (2*u*v, u2-v2, u2+v2)
    
primitives = [(a, b, c, a+b+c) | u <- [2..21], v <- [1..u], v < u, u `rem` 2 /= v `rem` 2, let (a, b, c) = primPythag u v, a+b+c <= 1000]

createFamilies [] _ = []
createFamilies p@((a, b, c, l):ps) n 
        | l * n > 1000 = createFamilies ps 1
        | otherwise = (a*n, b*n, c*n, l*n) : createFamilies p (n+1)

allPerimeters = sort( map (\(a,b,c,d)-> d) (createFamilies primitives 1)) 

mode l = mode' l 1 0 (head l)

mode' [] _ l v = v
mode' (x:[]) curr longest v = if curr > longest then x else v
mode' (x:xs) curr longest v 
        | x == head xs = mode' xs (curr+1) longest v
        | otherwise = if (curr > longest) then mode' xs 1 curr x
                      else mode' xs 1 longest v   
                      
euler39 = mode allPerimeters                       