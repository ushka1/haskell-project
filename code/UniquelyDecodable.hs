module UniquelyDecodable (uniquelyDecodable) where

import Helpers (dedupe, intersection, isPrefixOf, union)

-- --------------------------------------------------

-- | 'generateSuffixes' function generates a list of suffixes that can be added
-- to words in C_{from} set to create a word from C_{to} set.
generateSuffixes :: [String] -> [String] -> [String]
generateSuffixes cFrom cTo =
  [ drop (length y) x
    | x <- cTo,
      y <- cFrom,
      length x > length y,
      y `isPrefixOf` x
  ]

-- --------------------------------------------------

-- | 'generateCn' function generates a C_{n} set from a given C set.
generateCn :: [String] -> Int -> [String]
generateCn c 0 = dedupe c
generateCn c n = suf1 `union` suf2
  where
    cn_minus_1 = generateCn c (n - 1)
    -- Suffixes that can be added to suffixes in C_{N-1} set to create a word from C set.
    suf1 = generateSuffixes cn_minus_1 c
    -- Suffixes that can be added to words in C set to create a suffix from C_{N-1} set.
    suf2 = generateSuffixes c cn_minus_1

-- | 'generateCinf' function generates a C_{infinity} set from a given C set.
generateCinf :: [String] -> [String]
generateCinf c = generateCinfHelper c 1 [] []

-- | 'generateCinfHelper' function is a helper function for 'generateCinf'.
generateCinfHelper :: [String] -> Int -> [[String]] -> [String] -> [String]
generateCinfHelper c n cSets cinf =
  if null cn || cn `elem` cSets
    then cinf
    else generateCinfHelper c n' cSets' cinf'
  where
    cn = generateCn c n
    n' = n + 1
    cSets' = cn : cSets
    cinf' = cinf `union` cn

-- --------------------------------------------------

-- | 'uniquelyDecodable' function checks if a given set of words is uniquely decodable.
uniquelyDecodable :: [String] -> Bool
uniquelyDecodable c = null (c `intersection` cinf)
  where
    cinf = generateCinf c
