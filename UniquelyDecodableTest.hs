import UniquelyDecodable (uniquelyDecodable)

main :: IO ()
main = do
  let c = ["0", "01", "10", "011", "100", "101", "110"]
  print $ uniquelyDecodable c