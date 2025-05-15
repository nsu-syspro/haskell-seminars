import Control.Monad (void)
import Control.Exception

main :: IO ()
main = putStrLn "Hello World!"


foo :: IO ()
foo = putStrLn "foo"

-- [[1,4,6]]

-- * Do-notation

greeter :: IO ()
greeter = getLine >>= \name -> putStrLn ("Hello, " ++ name)

greeter' :: IO ()
greeter' = do
  name <- getLine
  let greeting  = "Hello, " ++ name
      greeting1 = greeting
      greeting2 = "Hello, " ++ name
  putStrLn greeting

ioFail :: IO ()
ioFail = void $ try @IOException $ readFile "bad"


