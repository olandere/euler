module Euler_56 where

import Char

intToList :: (Num a) => a -> [Int]
intToList n = map (\n-> ord n - 48) (show n)

digitalSum = sum . intToList

pairs' :: [(Integer, Integer)]
pairs' = [(a, b) | a <- [2..99], a `rem` 10 > 0, b <- [2..99], (digitalSum (exp' (a,b))) == 972]

pairs :: [(Integer, Integer)]
pairs = [(a, b) | a <- [2..99], a `rem` 10 > 0, b <- [2..99]]

exp' :: (Integer, Integer) -> Integer
exp' (a, b) = a ^ b

euler_56 = maximum (map digitalSum (map exp' pairs))
