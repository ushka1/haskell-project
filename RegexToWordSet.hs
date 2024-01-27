-- | intercalate "|" ["a", "b", "c"] = "a|b|c"
-- | intercalate "-*-" ["00", "00", "00"] = "00-*-00-*-00"
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x : xs) = x ++ sep ++ intercalate sep xs

-- | asciiRange 'a' 'g' = "abcdefg"
asciiRange :: Char -> Char -> String
asciiRange a b = map toEnum (enumFromTo (fromEnum a) (fromEnum b))

-- --------------------------------------------------

-- | convertAnyOfToOr "abc[def]ghi" = "abc(d|e|f)ghi"
-- | convertAnyOfToOr "abc[d-f]ghi" = "abc(d|e|f)ghi"
-- | TODO: convertAnyOfToOr "a[b-def-h]i" = "a(b|c|d|e|f|g|h)i"
convertAnyOfToOr :: String -> String
convertAnyOfToOr xs = convertAnyOfToOrHelper xs 0 [] ""

convertAnyOfToOrHelper ::
  String -> -- input
  Int -> -- state (0: outside of [], 1: inside of [])
  String -> -- characters of current []
  String -> -- accumulator (result)
  String
convertAnyOfToOrHelper "" i cur acc = acc
convertAnyOfToOrHelper (x : xs) i cur acc
  | x == '[' = convertAnyOfToOrHelper xs 1 [] acc
  | x == ']' = convertAnyOfToOrHelper xs 0 [] (acc ++ curFormatted)
  | i == 1 = convertAnyOfToOrHelper xs 1 (cur ++ [x]) acc
  | i == 0 = convertAnyOfToOrHelper xs 0 [] (acc ++ [x])
  | otherwise = error "convertAnyOfToOrHelper: invalid input"
  where
    curParsed =
      if (length cur == 3) && (cur !! 1 == '-')
        then asciiRange (head cur) (last cur)
        else cur
    curStringified = map (: []) curParsed
    curFormatted = "(" ++ intercalate "|" curStringified ++ ")"

-- --------------------------------------------------

-- | splitByOr "ab|cd|ef(ghi|jkl)" = ["ab", "cd", "ef(ghi|jkl)"]
splitByOr :: String -> [String]
splitByOr xs = reverse $ splitByOrHelper xs 0 "" []

splitByOrHelper :: String -> Int -> String -> [String] -> [String]
splitByOrHelper "" i cur acc = cur : acc
splitByOrHelper (x : xs) i cur acc
  | x == '(' = splitByOrHelper xs (i + 1) (x : cur) acc
  | x == ')' = splitByOrHelper xs (i - 1) (x : cur) acc
  | x == '|' && i == 0 = splitByOrHelper xs i "" (cur : acc)
  | otherwise = splitByOrHelper xs i (x : cur) acc

-- --------------------------------------------------

-- "a(b|c)d" -> ["a", "b|c", "d"] -> ["a", ["b", "c"], "d"] -> ["abd", "acd"]
-- parseGroup :: [String] -> [String]
