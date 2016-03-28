module Main where

import Data.List (permutations)

numbers :: [Int]
numbers = 0:[2..9]

addOne :: [Int] -> [Int]
addOne (a:b:c:d:e:f:g:h:_) = [a,b,c,d,e,f,g,h,1]

isValid :: [Int] -> Bool
isValid (a:b:c:d:e:f:g:h:p:_) =
    and [
        a /= 0,
        c /= 0,
        e /= 0,
        g /= 0,
        p /= 0,
        10 * a + b - 10 * c - d == 10 * e + f,
        10 * e + f + 10 * g + h == 111 * p]

answers :: [[Int]]
answers = filter isValid . map addOne . permutations $ numbers

main :: IO ()
main = putStrLn . show $ answers
