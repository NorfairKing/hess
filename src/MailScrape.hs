{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module MailScrape where

import qualified Data.ByteString      as SB
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB


import           Monad
import           TH
import           Types

mailPattern :: ByteString
mailPattern = LB.init [litFile|src/mailPattern.txt|]

mailScraper :: Pipe (URI, ByteString) ByteString IO ()
mailScraper = forever $ do
    (_, content) <- await
    let func = getAllTextMatches (content =~ mailPattern) :: [ByteString]
    mapM_ yield func

emailMatcher :: Pipe ByteString ByteString Spider ()
emailMatcher = forever $ do
    s <- await
    let func = getAllTextMatches (s =~ mailPattern) :: [ByteString]
    mapM_ yield func

validFilter :: Pipe ByteString ByteString Spider ()
validFilter = forever $ do
    m <- await
    let bs = SB.concat . LB.toChunks $ m
    when (isValid bs) $ yield m

