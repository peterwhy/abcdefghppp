module Main where

import Data.Maybe (catMaybes)

import MyDigits (digits)
import MyPermute (permute)

repeated :: Eq a => [a] -> Bool
repeated [] = False
repeated (x:xs) = (x `elem` xs) || repeated xs

calculateAll :: [Int] -> Maybe [Int]
calculateAll [a,b,c,d,p] = 
    let
        num3 = 10 * a + b - 10 * c - d
        digits3 = digits num3
        num4 = 111 * p - num3
        digits4 = digits num4 in
    if (and [10 <= num3, num3 < 100, 10 <= num4, num4 < 100])
        then Just $ [a,b,c,d] ++ digits3 ++ digits4 ++ [p]
        else Nothing

isValid :: [Int] -> Bool
isValid xs@[a,b,c,d,e,f,g,h,p] =
    and [
        a /= 0,
        c /= 0,
        e /= 0,
        g /= 0,
        p /= 0,
        not $ repeated xs,
        10 * a + b - 10 * c - d == 10 * e + f,
        10 * e + f + 10 * g + h == 111 * p]

answers :: [[Int]]
answers = filter isValid . catMaybes . map calculateAll . permute 5 $ [0..9]

main :: IO ()
main = putStrLn . show $ answers
