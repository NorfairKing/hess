{-# LANGUAGE OverloadedStrings #-}
module Store where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB

import           Types


store :: Consumer ByteString IO ()
store = appender

appender :: Consumer ByteString IO ()
appender = forever $ do
    bs <- await
    liftIO $ LB.appendFile "mail.txt" $ LB.concat [bs, "\n"]

printer :: Consumer ByteString IO ()
printer = forever $ do
    bs <- await
    liftIO $ LB.putStr bs
    liftIO $ LB.putStr "\n"

