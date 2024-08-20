import System.IO (hFlush, isEOF, stdout)

eval :: String -> String
eval = id

evalLoop :: IO ()
evalLoop =
  do
    putStr "> "
    hFlush stdout

    done <- isEOF
    if done
      then pure ()
      else do
        line <- getLine
        putStr $ eval line
        putStr "\n"
        evalLoop

main :: IO ()
main = evalLoop
