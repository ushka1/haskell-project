module Helpers
  ( intercalate,
    asciiRange,
    isPrefixOf,
    dedupe,
    intersection,
    union,
  )
where

-- | 'intercalate' function inserts a string between every element of a list and concatenates the result.
--
-- > intercalate "|" ["a", "b", "c"] = "a|b|c"
-- > intercalate "---" ["abc", "def", "ghi"] = "abc---def---ghi"
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x : xs) = x ++ sep ++ intercalate sep xs

-- | 'asciiRange' function returns a string of ASCII characters in the given range (inclusive).
--
-- > asciiRange 'a' 'i' = "abcdefghi"
asciiRange :: Char -> Char -> String
asciiRange a b = map toEnum (enumFromTo (fromEnum a) (fromEnum b))

-- --------------------------------------------------

-- | 'isPrefixOf' function checks if a given string is a prefix of another string.
isPrefixOf :: String -> String -> Bool
isPrefixOf "" _ = True
isPrefixOf _ "" = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- --------------------------------------------------

-- | 'dedupe' function removes duplicate elements from a list (also preserving the original order).
dedupe :: (Eq a) => [a] -> [a]
dedupe = reverse . foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | 'union' function returns the union of two lists.
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = dedupe (xs ++ ys)

-- | 'intersection' function returns the intersection of two lists.
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]
