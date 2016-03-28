module MyDigits (digits) where

digitsRev :: Int -> [Int]
digitsRev n
    | n < 0     = error "must be non-negative"
    | n == 0    = [0]
    | n < 10    = [n `mod` 10]
    | otherwise = (n `mod` 10) : digitsRev (n `div` 10)

digits :: Int -> [Int]
digits = reverse . digitsRev
