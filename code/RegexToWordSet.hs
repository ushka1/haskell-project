module RegexToWordSet (regexToWordSet) where

import Helpers (asciiRange, dedupe, intercalate)

-- --------------------------------------------------

-- | 'anyOfToOr' function converts a regex pattern that contains
--   character classes enclosed in square brackets to an equivalent pattern
--   that uses the or operator `|` instead.
--   For example, [abc] is equivalent to (a|b|c).
--   If the character class contains a range, such as [a-z], it expands it
--   to the list of all characters in that range, such as (a|b|...|z).
--
-- > anyOfToOr "abc[def]ghi" = "abc(d|e|f)ghi"
-- > anyOfToOr "abc[d-f]ghi" = "abc(d|e|f)ghi"
-- > TODO: anyOfToOr "a[b-def-h]i" = "a(b|c|d|e|f|g|h)i"
anyOfToOr :: String -> String
anyOfToOr xs = anyOfToOrHelper xs 0 [] ""

anyOfToOrHelper ::
  String -> -- regex pattern
  Int -> -- state (0: outside of set `[...]`, 1: inside of set `[...]`)
  String -> -- current set `[...]` content
  String -> -- result
  String
anyOfToOrHelper "" i cur res = res
anyOfToOrHelper (x : xs) i cur res
  | x == '[' = anyOfToOrHelper xs 1 [] res
  | x == ']' = anyOfToOrHelper xs 0 [] (res ++ curFormatted)
  | i == 1 = anyOfToOrHelper xs 1 (cur ++ [x]) res
  | i == 0 = anyOfToOrHelper xs 0 [] (res ++ [x])
  | otherwise = error "anyOfToOrHelper: invalid input"
  where
    curParsed =
      if (length cur == 3) && (cur !! 1 == '-')
        then asciiRange (head cur) (last cur)
        else cur
    curStringified = map (: []) curParsed
    curFormatted = "(" ++ intercalate "|" curStringified ++ ")"

-- --------------------------------------------------

-- | 'splitByOr' function splits a regex pattern by the or operator `|`
--   and returns a list of subpatterns. It takes into account the level
--   of parentheses nesting and does not split inside a group.
--
-- > splitByOr "ab|cd|ef" = ["ab", "cd", "ef"]
-- > splitByOr "a|b(c|d)" = ["a", "b(c|d)"]
splitByOr :: String -> [String]
splitByOr xs = splitByOrHelper xs 0 "" []

splitByOrHelper ::
  String -> -- regex pattern
  Int -> -- level of `(...)` nesting
  String -> -- current string before `|`
  [String] -> -- result
  [String]
splitByOrHelper "" i cur res = res ++ [cur]
splitByOrHelper (x : xs) i cur res
  | x == '(' = splitByOrHelper xs (i + 1) (cur ++ [x]) res
  | x == ')' = splitByOrHelper xs (i - 1) (cur ++ [x]) res
  | x == '|'
      && i == 0
      && not (null cur)
      && not (null xs) =
      splitByOrHelper xs i "" (res ++ [cur])
  | otherwise = splitByOrHelper xs i (cur ++ [x]) res

-- --------------------------------------------------

-- | 'splitByGroup' function splits a regex pattern by the groups enclosed
--   in parentheses and returns a list of subpatterns. It takes into account
--   the level of parentheses nesting and does not split inside a group.
--
-- > splitByGroup "a(b|c)d" = ["a", "b|c", "d"]
-- > splitByGroup "a(b(c|d)e)f" = ["a", "b(c|d)e", "f"]
splitByGroup :: String -> [String]
splitByGroup xs = splitByGroupHelper xs 0 "" []

splitByGroupHelper ::
  String -> -- regex pattern
  Int -> -- level of `(...)` nesting
  String -> -- current group `(...)` content
  [String] -> -- result
  [String]
splitByGroupHelper "" i "" res = res
splitByGroupHelper "" i cur res = res ++ [cur]
splitByGroupHelper (x : xs) i cur res
  | x == '(' && i == 0 && null cur = splitByGroupHelper xs (i + 1) [] res
  | x == '(' && i == 0 = splitByGroupHelper xs (i + 1) [] (res ++ [cur])
  | x == ')' && i == 1 && null cur = splitByGroupHelper xs (i - 1) [] res
  | x == ')' && i == 1 = splitByGroupHelper xs (i - 1) [] (res ++ [cur])
  | x == '(' = splitByGroupHelper xs (i + 1) (cur ++ [x]) res
  | x == ')' = splitByGroupHelper xs (i - 1) (cur ++ [x]) res
  | otherwise = splitByGroupHelper xs i (cur ++ [x]) res

-- --------------------------------------------------

regexToWordSet :: String -> [String]
regexToWordSet xs
  | null xs = []
  | not (hasGroups || hasOrs) = [xs]
  | otherwise = result
  where
    hasGroups = '(' `elem` xs
    hasOrs = '|' `elem` xs

    ors = splitByOr xs
    groups = map splitByGroup ors
    combintations = map (map regexToWordSet) groups
    cartesianProduct = map (foldr1 (\xs ys -> [x ++ y | x <- xs, y <- ys])) combintations
    result = foldr1 (++) cartesianProduct
