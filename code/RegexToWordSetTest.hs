module RegexToWordSetTest where

import Helpers (qsort)
import RegexToWordSet (regexToWordSet)

runTests :: IO ()
runTests = do
  let testCases =
        [ ("a|b|c", ["a", "b", "c"]),
          ("(a|b)cde", ["acde", "bcde"]),
          ("(a|b)|c", ["a", "b", "c"]),
          ("abc|(de|ef)", ["abc", "de", "ef"]),
          ("(a|b)(c|d)(e|f)", ["ace", "acf", "ade", "adf", "bce", "bcf", "bde", "bdf"]),
          ("(((a)|(b))|((c)|(d))|((e)|(f)))", ["a", "b", "c", "d", "e", "f"]),
          ("a|(b|(c|(d|e)))", ["a", "b", "c", "d", "e"]),
          ("a(b|c(d|e(f|g)))", ["ab", "acd", "acef", "aceg"]),
          ("[a-cA-C]", ["a", "b", "c", "A", "B", "C"]),
          ("(a|b)[0-2]c", ["a0c", "a1c", "a2c", "b0c", "b1c", "b2c"]),
          ("a[0-1]([b-c]|[d-e])", ["a0b", "a0c", "a0d", "a0e", "a1b", "a1c", "a1d", "a1e"])
        ]
  mapM_ runTest testCases
  where
    runTest (input, expected) = do
      let result = regexToWordSet input
      if qsort result == qsort expected
        then putStrLn $ "Test passed for: " ++ show input
        else putStrLn $ "Test failed for: " ++ show input ++ ", expected " ++ show expected ++ ", but got " ++ show result
