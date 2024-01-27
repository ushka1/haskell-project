import UniquelyDecodable (uniquelyDecodable)

main :: IO ()
main = do
  let testCases =
        [ -- Uniquely decodable test cases.
          (["0", "1"], True),
          (["0", "01", "11"], True),
          (["0", "10", "110", "111"], True),
          (["0", "01", "011", "1110"], True),
          (["0", "10", "110", "1110", "11110"], True),
          (["0101", "1001", "10", "000", "11", "100"], True),
          (["car", "pet", "carpets"], True),
          (["cars", "pet", "carpet"], True),
          -- Not uniquely decodable test cases.
          (["0", "00"], False),
          (["0", "01", "10"], False),
          (["0", "10", "010", "101"], False),
          (["0", "001", "010", "100"], False),
          (["02", "12", "120", "20", "21"], False),
          (["1", "011", "01110", "1110", "10011"], False),
          (["car", "pet", "carpet"], False),
          (["car", "pets", "carpets"], False)
        ]
  mapM_ runTest testCases
  where
    runTest (input, expected) = do
      let result = uniquelyDecodable input
      if result == expected
        then putStrLn $ "Test passed for: " ++ show input
        else putStrLn $ "Test failed for: " ++ show input ++ ", expected " ++ show expected ++ ", but got " ++ show result
