-- exercise 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p [] = []
splitWith p xs = let (pre, suf) = span p xs
                 in pre : splitWith p (dropWhile (not . p) suf)

-- exercise 3
firstWords :: String -> String
firstWords = unlines . (map (first . words)) . lines
    where first [] = []
          first (x:_) = x

-- exercise 4
split :: [a] -> (a, [a])
split (x:xs) = (x,xs)

splits :: [[a]] -> ([a], [[a]])
splits = (\ps -> (map fst ps, map snd ps)) . map split

swap :: [[a]] -> [[a]]
swap [] = []
swap xss = if any null xss
           then []
           else
            let (firsts, rests) = splits xss
            in firsts : swap rests

pad :: a -> [[a]] -> [[a]]
pad x xss = map (contract . expand) xss
    where expand xs = xs ++ repeat x
          contract = take m
          m = maximum $ map length xss

transpose :: String -> String
transpose = unlines . swap . (pad ' ') . lines

-- left fold using right fold
-- to produce the desired left associativity, we first construct a function by
-- stacking (pre-composing) user fold operations from right to left, and then
-- apply this function to the user initial value
foldL :: (a -> b -> a) -> a -> [b] -> a
foldL f z xs = foldr (\x g -> g . flip f x) id xs z
