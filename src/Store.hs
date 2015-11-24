{-# LANGUAGE OverloadedStrings #-}
module Store where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB

import           Monad
import           Types


store :: Consumer ByteString HESS ()
store = appender

appender :: Consumer ByteString HESS ()
appender = forever $ do
    bs <- await
    f <- view mail_store
    liftIO $ LB.appendFile f $ LB.concat [bs, "\n"]

printer :: Consumer ByteString HESS ()
printer = forever $ do
    bs <- await
    liftIO $ LB.putStr bs
    liftIO $ LB.putStr "\n"

