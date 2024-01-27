import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let input = head args
  putStrLn input
