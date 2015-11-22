module Utils where

import           Types

yieldMaybe :: (Monad m) => Maybe b -> Proxy a' a () b m ()
yieldMaybe Nothing = return ()
yieldMaybe (Just a) = yield a

fstPicker :: Monad m => Pipe (a, b) a m ()
fstPicker = forever $ do
    (a, _) <- await
    yield a

sndPicker :: Monad m => Pipe (a, b) b m ()
sndPicker = forever $ do
    (_, b) <- await
    yield b
