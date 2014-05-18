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

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs = if any null xs then [] else
                let (firsts, rests) = splits xs
                in firsts : transpose rests

transpose' :: String -> String
transpose' = unlines . transpose . lines