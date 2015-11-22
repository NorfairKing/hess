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

mailScraper :: Pipe ByteString ByteString IO ()
mailScraper = forever $ do
    content <- await
    let func = getAllTextMatches (content =~ mailPattern) :: [ByteString]
    mapM_ yield func

validFilter :: Pipe ByteString ByteString IO ()
validFilter = forever $ do
    m <- await
    let bs = SB.concat . LB.toChunks $ m
    when (isValid bs) $ yield m

