module MyPermute (permute, permutations) where

permute :: Int -> [a] -> [[a]]
permute n = permuteInternal n [] []

permutations :: [a] -> [[a]]
permutations xs = permute (length xs) xs

permuteInternal :: Int -> [a] -> [a] -> [a] -> [[a]]
permuteInternal 0 takens _ _ = [reverse takens]
permuteInternal n _ _ [] = []
permuteInternal n takens heads (tail:tails) =
    permuteInternal (n-1) (tail:takens) [] (reverse heads ++ tails)
    ++ permuteInternal n takens (tail:heads) tails
