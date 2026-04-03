import Data.Functor (void)

main :: IO ()
main = putStrLn "Hello" <* putStrLn "World!"

foo :: IO ()
foo = putStrLn "Hello World2!"

bar :: IO String
bar = getLine
