
module Euler_40 where

import Char

intToList n = map (\n-> ord n - 48) (show n)

numList = [x | n <- [1..], x <- intToList n]

funcD n = head (drop (n-1) numList)

euler40 = (funcD 1) * (funcD 10) * (funcD 100) * (funcD 1000) * (funcD 10000) *
          (funcD 100000) * (funcD 1000000)