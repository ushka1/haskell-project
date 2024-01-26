-- | 'isPrefixOf' function checks if a given string is a prefix of another string.
isPrefixOf :: String -> String -> Bool
isPrefixOf "" _ = True
isPrefixOf _ "" = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- | 'dedupe' function removes duplicate elements from a list.
dedupe :: (Eq a) => [a] -> [a]
dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | 'union' function returns the union of two lists.
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = dedupe (xs ++ ys)

-- | 'intersection' function returns the intersection of two lists.
intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, x `elem` ys]

-- --------------------------------------------------

-- | 'suffixes' function generates all suffixes of a given string.
suffixes :: String -> [String]
suffixes "" = []
suffixes (_ : "") = []
suffixes (_ : xs) = xs : suffixes xs

-- | 'suffixesOfEach' function generates all suffixes of each string in a list.
suffixesOfEach :: [String] -> [String]
-- suffixesOfEach xs = dedupe (concat (map (\x -> suffixes x) xs))
-- suffixesOfEach xs = dedupe (concat (map suffixes xs))
-- suffixesOfEach xs = (dedupe . concat . (map suffixes)) xs
-- suffixesOfEach = dedupe . concat . (map suffixes)
suffixesOfEach = dedupe . concatMap suffixes

-- --------------------------------------------------

-- | 'generateCn' function generates a C_{n} set from a given C_{0} set.
generateCn :: [String] -> Int -> [String]
generateCn c0 0 = dedupe c0
generateCn c0 n = suf1 `union` suf2
  where
    cn_minus_1 = generateCn c0 (n - 1)
    suf = suffixesOfEach c0
    suf1 = [s | c <- c0, s <- suf, (c ++ s) `elem` cn_minus_1]
    suf2 = [s | c <- cn_minus_1, s <- suf, (c ++ s) `elem` c0]

-- | 'generateCinf' function generates a C_{inf} set from a given C_{0} set.
generateCinf :: [String] -> [String]
generateCinf c0 = generateCinfHelper c0 1 [] []

-- | 'generateCinfHelper' function is a helper function for 'generateCinf'.
generateCinfHelper :: [String] -> Int -> [[String]] -> [String] -> [String]
generateCinfHelper c0 n cs ci =
  if null cn || cn `elem` cs
    then ci
    else generateCinfHelper c0 (n + 1) (cn : cs) (cn `union` ci)
  where
    cn = generateCn c0 n

-- --------------------------------------------------

-- | 'decodable' function checks if a given set of words is uniquely decodable.
decodable :: [String] -> Bool
decodable c0 = null (c0 `intersection` cinf)
  where
    cinf = generateCinf c0

-- decodable ["0", "01", "11"]
-- True

-- decodable ["0", "10", "11"]
-- True

-- decodable ["0", "01", "10"]
-- False

-- decodable ["02", "12", "120", "20", "21"]
-- False

-- decodable ["10", "20", "11", "2", "0"]
-- False

-- decodable ["car", "pet", "carpet"]
-- False