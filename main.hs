module Main where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Text.Printf (printf)

import MyDigits (digits, digitsRev, unDigits)
import MyPermute (permute)

repeated :: Eq a => [a] -> Bool
repeated [] = False
repeated (x:xs) = (x `elem` xs) || repeated xs

calculateAll :: [Int] -> Maybe [Int]
calculateAll [a,b,c,d,p] = 
    let
        num1 = unDigits 10 [a,b]
        num2 = unDigits 10 [c,d]
        num3 = num1 - num2
        num4 = 111 * p - num3
        nums = [num1, num2, num3, num4] in
    if ((and . map (inRange 10 2) $ nums) && inRange 10 1 p)
        then Just $ [a,b,c,d] ++ digits 10 num3 ++ digits 10 num4 ++ [p]
        else Nothing
calculateAll _ = error "wrong length"

inRange :: Int -> Int -> Int -> Bool
inRange base len n = n > 0 && (length . digitsRev base $ n) == len

isValid :: Int -> Int -> [Int] -> Bool
isValid base len xs
    | length xs == 4 * len + 1 = not $ repeated xs
isValid _ _ _ = error "wrong length"

answers :: [[Int]]
answers = filter (isValid 10 2) . catMaybes . map calculateAll . permute 5 $ [0..9]

format :: [Int] -> String
format [a,b,c,d,e,f,g,h,p] =
    let
        lines = [
            "  %d%d", "- %d%d", "----",
            "  %d%d", "+ %d%d", "----", " %d%d%d", ""]
        template = intercalate "\n" lines in
    printf template a b c d e f g h p p p
format _ = error "wrong length"

main :: IO ()
main = putStrLn . intercalate "====\n" . map format $ answers
