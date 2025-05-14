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
  putStrLn ("Hello, " ++ name)
