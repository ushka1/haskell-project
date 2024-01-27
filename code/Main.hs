import RegexToWordSet (regexToWordSet)
import System.Environment
import UniquelyDecodable (uniquelyDecodable)

main :: IO ()
main = do
  args <- getArgs
  let regex = head args
  let wordSet = regexToWordSet regex
  let decodable = uniquelyDecodable wordSet
  putStrLn $ "Code: " ++ show wordSet
  putStrLn $ "Uniquely decodable: " ++ show decodable