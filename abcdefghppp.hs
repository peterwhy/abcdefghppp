module Abcdefghppp where

import Data.List (permutations)

type Tuple = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

numbers :: [Int]
numbers = 0:[2..9]

toTuple :: [Int] -> Tuple
toTuple (a:b:c:d:e:f:g:h:_) = (a,b,c,d,e,f,g,h,1)

isValid :: Tuple -> Bool
isValid (a,b,c,d,e,f,g,h,p) =
    and [
        a /= 0,
        c /= 0,
        e /= 0,
        g /= 0,
        p /= 0,
        10 * a + b - 10 * c - d == 10 * e + f,
        10 * e + f + 10 * g + h == 111 * p]

answers :: [Tuple]
answers = filter isValid . map toTuple . permutations $ numbers
