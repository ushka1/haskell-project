import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let regex = head args
  putStrLn $ "Regex: " ++ regex