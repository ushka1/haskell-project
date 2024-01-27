module Helpers
  ( asciiRange,
    isPrefixOf,
    intercalate,
    splitByDelimiter,
    dedupe,
    union,
    intersection,
    qsort,
  )
where

-- -------------------- Strings --------------------

-- | 'asciiRange' function returns a string of ASCII characters in the given range (inclusive).
--
-- > asciiRange 'a' 'i' = "abcdefghi"
asciiRange :: Char -> Char -> String
asciiRange a b = map toEnum (enumFromTo (fromEnum a) (fromEnum b))

-- | 'isPrefixOf' function checks if a given string is a prefix of another string.
isPrefixOf :: String -> String -> Bool
isPrefixOf "" _ = True
isPrefixOf _ "" = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- | 'intercalate' function inserts a string between every element of a list and concatenates the result.
--
-- > intercalate "|" ["a", "b", "c"] = "a|b|c"
-- > intercalate "---" ["abc", "def", "ghi"] = "abc---def---ghi"
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x : xs) = x ++ sep ++ intercalate sep xs

-- | 'splitByDelimiter' function splits a string by a given delimiter and returns a list of substrings.
--
-- > splitByDelimiter '-' "a-zA-Z" = ["a", "zA", "z"]
splitByDelimiter :: Char -> String -> [String]
splitByDelimiter _ "" = []
splitByDelimiter del xs = splitByDelimiterHelper del xs "" []

splitByDelimiterHelper :: Char -> String -> String -> [String] -> [String]
splitByDelimiterHelper _ "" cur res = res ++ [cur]
splitByDelimiterHelper del (x : xs) cur res
  | x == del = splitByDelimiterHelper del xs "" (res ++ [cur])
  | otherwise = splitByDelimiterHelper del xs (cur ++ [x]) res

-- -------------------- Lists/Sets --------------------

-- | 'dedupe' function removes duplicate elements from a list also preserving the original order.
dedupe :: (Eq a) => [a] -> [a]
dedupe [] = []
dedupe (x : xs) = x : dedupe (filter (/= x) xs)

-- | 'union' function returns the union of two lists.
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = dedupe (xs ++ ys)

-- | 'intersection' function returns the intersection of two lists.
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]

-- | 'qsort' function sorts a list using the quicksort algorithm.
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort [a | a <- xs, a < x] ++ [x] ++ qsort [b | b <- xs, b >= x]