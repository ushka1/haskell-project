module RegexToWordSet (regexToWordSet) where

import Helpers (asciiRange, dedupe, intercalate)

-- --------------------------------------------------

-- | 'expandSet' function converts a regex pattern that contains
--   character classes enclosed in square brackets to an equivalent pattern
--   that uses the or operator `|` instead.
--   For example, [abc] is equivalent to (a|b|c).
--   If the character class contains a range, such as [a-z], it expands it
--   to the list of all characters in that range, such as (a|b|...|z).
--
-- > expandSet "abc[def]ghi" = "abc(d|e|f)ghi"
-- > expandSet "abc[d-f]ghi" = "abc(d|e|f)ghi"
-- > TODO: expandSet "a[b-def-h]i" = "a(b|c|d|e|f|g|h)i"
expandSet :: String -> String
expandSet xs = expandSetHelper xs 0 [] ""

expandSetHelper ::
  String -> -- regex pattern
  Int -> -- state (0: outside of set `[...]`, 1: inside of set `[...]`)
  String -> -- current set `[...]` content
  String -> -- result
  String
expandSetHelper "" _ _ res = res
expandSetHelper (x : xs) i cur res
  | x == '[' = expandSetHelper xs 1 [] res
  | x == ']' = expandSetHelper xs 0 [] (res ++ curFormatted)
  | i == 1 = expandSetHelper xs 1 (cur ++ [x]) res
  | i == 0 = expandSetHelper xs 0 [] (res ++ [x])
  | otherwise = error "expandSetHelper: invalid input"
  where
    curParsed =
      if (length cur == 3) && (cur !! 1 == '-')
        then asciiRange (head cur) (last cur)
        else cur
    curStringified = map (: []) curParsed
    curFormatted = "(" ++ intercalate "|" curStringified ++ ")"

-- --------------------------------------------------

-- | 'splitOr' function splits a regex pattern by the or operator `|`
--   and returns a list of subpatterns. It takes into account the level
--   of parentheses nesting and does not split inside a group.
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
--   in parentheses and returns a list of subpatterns. It takes into account
--   the level of parentheses nesting and does not split inside a group.
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
-- > regexToWordSet "a(b|c)d" = ["abd", "acd"]
-- > regexToWordSet "a(b|c|e)d" = ["abd", "acd", "aed"]
regexToWordSet :: String -> [String]
regexToWordSet xs
  | null xs = []
  | not (hasOr xs) && not (hasGroups xs) = [xs]
  | otherwise = result
  where
    disjointed = splitOr xs
    ungrouped = map splitGroup disjointed
    recursive = map (map regexToWordSet) ungrouped
    combined = map combineStringLists recursive
    result = dedupe $ foldr1 (++) combined

{-
  Example of regexToWordSet flow:
  1. input = "a|b(c|d)|e"
  2. disjointed = ["a", "b(c|d)", "e"]
  3. ungrouped = [["a"], ["b", "c|d"], ["e"]]
  4. recursive = ... = [[["a"]], [["b"], ["c", "d"]], [["e"]]]
  5. combined = [["a"], ["bc", "bd"], ["e"]]
  6. result = ["a", "bc", "bd", "e"]

  Example of regexToWordSet flow:
  1. input = "(a|b)(c|d)"
  2. disjointed = ["(a|b)(c|d)"]
  3. ungrouped = [["a|b", "c|d"]]
  4. recursive = ... = [[["a"], ["b"]], [["c"], ["d"]]]
  5. combined = [["ac", "ad", "bc", "bd"]]
  6. result = ["ac", "ad", "bc", "bd"]
-}

regexToWordSetRunner :: String -> [String]
regexToWordSetRunner xs = regexToWordSet (expandSet xs)