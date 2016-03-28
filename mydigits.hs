module MyDigits (digits, unDigits, digitsRev) where

digitsRev :: Integral n => n -> n -> [n]
digitsRev base n
    | n < 0     = error "must be non-negative"
    | n == 0    = [0]
    | n < base  = [n `mod` base]
    | otherwise = (n `mod` base) : digitsRev base (n `div` base)

digits :: Integral n => n -> n -> [n]
digits base = reverse . digitsRev base

unDigits :: Integral n => n -> [n] -> n
unDigits base = foldl (\a b -> base * a + b) 0 
