module UniquelyDecodableTest where

import UniquelyDecodable (uniquelyDecodable)

main :: IO ()
main = do
  let testCases =
        [ (["0", "01", "11"], True),
          (["0", "01", "10"], False),
          (["0", "10", "110", "111"], True),
          (["0", "10", "010", "101"], False),
          (["0", "2", "10", "20", "11"], False),
          (["02", "12", "120", "20", "21"], False),
          (["car", "pet", "carpet"], False)
        ]
  mapM_ runTest testCases
  where
    runTest (input, expected) = do
      let result = uniquelyDecodable input
      if result == expected
        then putStrLn $ "Test passed for: " ++ show input
        else putStrLn $ "Test failed for: " ++ show input ++ ", expected " ++ show expected ++ ", but got " ++ show result
