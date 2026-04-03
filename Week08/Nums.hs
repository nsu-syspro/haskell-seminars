import Data.Foldable (sequenceA_, traverse_)
import Control.Concurrent (forkIO, threadDelay)
import System.IO
import Data.Functor (void)

main :: IO ()
main =
     hSetBuffering stdout NoBuffering
  *> traverse_ forkIO
     [ printList [1..10]
     , printList ['a'..'z']
     , printList ["abc", "bac", "cba"]
     ]
  *> threadDelay 1000

--main :: IO ()
--main =
--     hSetBuffering stdout NoBuffering
--  *> forkIO (printList [1..10])
--  *> forkIO (printList ['a'..'z'])
--  *> void (forkIO (printList ["abc", "bac", "cba"]))
--  *> threadDelay 1000

printN :: Int -> IO ()
printN 0 = pure ()
printN n = print n *> printN (n - 1)

printN' :: Int -> IO ()
printN' 0 = pure ()
printN' n = printN' (n - 1) *> print n

-- print 1 *> print 2 *> ... *> print 10

printNums :: [Int] -> IO ()
printNums xs = sequenceA_ prints
--printNums xs = foldr (*>) (pure ()) prints
 where
  prints :: [IO ()]
  prints = map print xs

printList :: Show a => [a] -> IO ()
printList = traverse_ print
--printList xs = traverse_ print xs
--printList xs = sequenceA_ (map print xs)


