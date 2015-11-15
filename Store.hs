{-# LANGUAGE OverloadedStrings #-}
module Store where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB

import           Monad
import           Types


printer :: Consumer ByteString Spider ()
printer = forever $ do
    bs <- await
    liftIO $ LB.putStr bs
    liftIO $ LB.putStr "\n"

