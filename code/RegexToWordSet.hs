module RegexToWordSet (regexToWordSet) where

import Helpers (asciiRange, dedupe, intercalate, splitByDelimiter)

-- --------------------------------------------------

-- | 'expandClass' function converts a regex pattern that contains
-- character classes enclosed in square brackets to an equivalent pattern
-- that uses the or `|` operator instead.
--
-- For example, [abc] is equivalent to (a|b|c). If the character class
-- contains a range, such as [a-z], it expands it to the list of all characters
-- in that range, such as (a|b|...|z).
--
-- > expandClass "abc[def]ghi" = "abc(d|e|f)ghi"
-- > expandClass "abc[d-f]ghi" = "abc(d|e|f)ghi"
-- > expandClass "a[b-def-h]i" = "a(b|c|d|e|f|g|h)i"
expandClass :: String -> String
expandClass xs = expandClassHelper xs 0 [] ""

expandClassHelper ::
  String -> -- regex pattern
  Int -> -- state (0: outside of class `[...]`, 1: inside of class `[...]`)
  String -> -- current class `[...]` content
  String -> -- result
  String
expandClassHelper "" _ _ res = res
expandClassHelper (x : xs) i cur res
  | x == '[' = expandClassHelper xs 1 [] res
  | x == ']' = expandClassHelper xs 0 [] (res ++ curFormatted)
  | i == 1 = expandClassHelper xs 1 (cur ++ [x]) res
  | i == 0 = expandClassHelper xs 0 [] (res ++ [x])
  | otherwise = error "expandClassHelper: invalid input"
  where
    -- Operations on the current class `[...]` content.
    curExpanded = expandRanges cur
    curDeduped = dedupe curExpanded
    curStringified = map (: []) curDeduped
    curFormatted = "(" ++ intercalate "|" curStringified ++ ")"

-- | 'expandRanges' function converts a regex pattern that contains ranges to equivalent string
-- that contains all characters in those ranges.
--
-- > expandRanges "a-z" = "abc...xyz"
-- > expandRanges "a-z0-9" = "abc...xyz012...789"
-- > expandRanges "012a-z345" = "012abc...xyz345"
expandRanges :: String -> String
expandRanges xs =
  foldl
    ( \acc x ->
        if not (null acc)
          then init acc ++ asciiRange (last acc) (head x) ++ tail x
          else x
    )
    ""
    (splitByDelimiter '-' xs)

-- --------------------------------------------------

-- | 'splitOr' function splits a regex pattern by the or operator `|`
-- and returns a list of subpatterns. It takes into account the level
-- of parentheses nesting and does not split inside a group.
--
-- > splitOr "ab|cd|ef" = ["ab", "cd", "ef"]
-- > splitOr "a|b(c|d)" = ["a", "b(c|d)"]
splitOr :: String -> [String]
splitOr xs = splitOrHelper xs 0 "" []

splitOrHelper ::
  String -> -- regex pattern
  Int -> -- level of `(...)` nesting
  String -> -- current string before `|`
  [String] -> -- result
  [String]
splitOrHelper "" _ "" res = res
splitOrHelper "" _ cur res = res ++ [cur]
splitOrHelper (x : xs) i cur res
  | x == '(' = splitOrHelper xs (i + 1) (cur ++ [x]) res
  | x == ')' = splitOrHelper xs (i - 1) (cur ++ [x]) res
  | x == '|' && i == 0 && null cur = splitOrHelper xs i "" res
  | x == '|' && i == 0 = splitOrHelper xs i "" (res ++ [cur])
  | otherwise = splitOrHelper xs i (cur ++ [x]) res

-- | 'hasOr' function checks if a regex pattern contains the or operator `|`.
hasOr :: String -> Bool
hasOr xs = '|' `elem` xs

-- --------------------------------------------------

-- | 'splitGroup' function splits a regex pattern by the groups enclosed
-- in parentheses and returns a list of subpatterns. It takes into account
-- the level of parentheses nesting and does not split inside a group.
--
-- > splitGroup "a(b|c)d" = ["a", "b|c", "d"]
-- > splitGroup "a(b(c|d)e)f" = ["a", "b(c|d)e", "f"]
splitGroup :: String -> [String]
splitGroup xs = splitGroupHelper xs 0 "" []

splitGroupHelper ::
  String -> -- regex pattern
  Int -> -- level of `(...)` nesting
  String -> -- current group `(...)` content
  [String] -> -- result
  [String]
splitGroupHelper "" _ "" res = res
splitGroupHelper "" _ cur res = res ++ [cur]
splitGroupHelper (x : xs) i cur res
  | x == '(' && i == 0 && null cur = splitGroupHelper xs (i + 1) [] res
  | x == '(' && i == 0 = splitGroupHelper xs (i + 1) [] (res ++ [cur])
  | x == ')' && i == 1 && null cur = splitGroupHelper xs (i - 1) [] res
  | x == ')' && i == 1 = splitGroupHelper xs (i - 1) [] (res ++ [cur])
  | x == '(' = splitGroupHelper xs (i + 1) (cur ++ [x]) res
  | x == ')' = splitGroupHelper xs (i - 1) (cur ++ [x]) res
  | otherwise = splitGroupHelper xs i (cur ++ [x]) res

-- | 'hasGroups' function checks if a regex pattern contains groups enclosed in parentheses.
hasGroups :: String -> Bool
hasGroups xs = '(' `elem` xs

-- --------------------------------------------------

-- | 'combineStringLists' takes a list of lists of strings and returns a list of
-- all possible combinations of those strings. Each combination is a concatenation
-- of one string from each list, in the order the lists appear in the input.
--
-- > combineStringLists [["a"], ["b", "c"], ["d"]] = ["abd", "acd"]
-- > combineStringLists [["a", "b", "c"], ["d"]] = ["ad", "bd", "cd"]
combineStringLists :: [[String]] -> [String]
combineStringLists = foldr1 (\xs ys -> [x ++ y | x <- xs, y <- ys])

-- --------------------------------------------------

-- | 'regexToWordSet' is a function that transforms a regex pattern into a set of words.
--
-- Supported regex operators:
-- - `ab|cd` (or)
-- - `(abc)` (group)
-- - `[abc]` (character class)
-- - `[a-z]` (character class with range)
-- - `[a-zA-Z]` (character class with multiple ranges)
--
-- Currently not supported operators:
-- - `a?` (optional)
-- - `a{m}` (quantifier)
-- - `a{m,}` (quantifier)
-- - `a{m,n}` (quantifier)
--
-- > regexToWordSet "a(b|c)d" = ["abd", "acd"]
-- > regexToWordSet "a(b|c|e)d" = ["abd", "acd", "aed"]
-- > regexToWordSet "a[b-d]e" = ["abe", "ace", "ade"]
regexToWordSet :: String -> [String]
regexToWordSet xs = regexToWordSetHelper (expandClass xs)

-- | 'regexToWordSetHelper' is a helper function for 'regexToWordSet' that
-- transforms a regex pattern into a set of words handling "|" and "()" operators.
--
-- The flow of the function is as follows:
-- 1. Split the regex pattern by `|` to get a list of disjointed subpatterns.
-- 2. Split each subpattern by `(...)` to get a list of ungrouped subpatterns.
-- 3. Recursively call `regexToWordSetHelper` on each ungrouped subpattern.
-- 4. Combine the results of the recursive calls.
-- 5. Dedupe and flatten the combined results.
-- 6. Return the result.
--
-- Flow example 1:
-- 1. regex pattern = "a|b(c|d)|e"
-- 2. disjointed = ["a", "b(c|d)", "e"]
-- 3. ungrouped = [["a"], ["b", "c|d"], ["e"]]
-- 4. recursive = ... = [[["a"]], [["b"], ["c", "d"]], [["e"]]]
-- 5. combined = [["a"], ["bc", "bd"], ["e"]]
-- 6. result = ["a", "bc", "bd", "e"]
--
-- Flow example 2:
-- 1. regex pattern = "(a|b)(c|d)"
-- 2. disjointed = ["(a|b)(c|d)"]
-- 3. ungrouped = [["a|b", "c|d"]]
-- 4. recursive = ... = [[["a"], ["b"]], [["c"], ["d"]]]
-- 5. combined = [["ac", "ad", "bc", "bd"]]
-- 6. result = ["ac", "ad", "bc", "bd"]
regexToWordSetHelper :: String -> [String]
regexToWordSetHelper xs
  | null xs = []
  | not (hasOr xs) && not (hasGroups xs) = [xs]
  | otherwise = result
  where
    -- Operations on the regex pattern.
    disjointed = splitOr xs
    ungrouped = map splitGroup disjointed
    recursive = map (map regexToWordSetHelper) ungrouped
    combined = map combineStringLists recursive
    result = dedupe $ foldr1 (++) combined
