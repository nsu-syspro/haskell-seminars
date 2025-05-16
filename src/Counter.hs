import StateT

import Control.Monad (void)

-- * Counter app
--
-- Displays current counter value
-- and increments it each time user presses button (e.g. enter)
main :: IO ()
main = void $ runStateT counter 0

type Counter a = StateT Int IO a

increment :: Counter Int
increment = modify (+1) *> get

increment' :: MonadState Int m => m Int
increment' = modify (+1) *> get

counter :: Counter ()
counter = do -- Counter
  v <- get
  liftIO $ do -- IO
    putStrLn ("Counter: " ++ show v)
    getLine
  increment
  counter


-- See https://www.williamyaoh.com/posts/2023-06-10-monad-transformers-101.html
