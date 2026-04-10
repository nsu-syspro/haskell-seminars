
main :: IO ()
main = greet

greet :: IO ()
greet =
      putStrLn "Enter your name: "
  >>  getLine
  >>= (\name -> putStrLn ("Hello " <> name))

greet' :: IO ()
greet' = do
  putStrLn "Enter your name: "
  name <- getLine
  let foo = id
  putStrLn ("Hello " <> foo name)
