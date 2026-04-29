import StateT

-- * Counter app
--
-- Displays current counter value
-- and increments it each time user presses button (e.g. enter)
main = evalStateT counter 0

counter :: StateT Int IO ()
counter = do
  cur <- get
  --StateT $ \s -> (putStrLn ("Counter: " <> show cur), s)
  lift $ do -- IO
    putStrLn ("Counter: " <> show cur)
    getLine
  modify (+1)
  counter

--counter :: (MonadIO m, MonadState Int m) => m ()
--counter = do
--  cur <- get
--  --StateT $ \s -> (putStrLn ("Counter: " <> show cur), s)
--  lift $ do -- IO
--    putStrLn ("Counter: " <> show cur)
--    getLine
--  modify (+1)
--  counter
