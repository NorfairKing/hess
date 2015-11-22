{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module MailScrape where

import qualified Data.ByteString      as SB
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB


import qualified Data.Set             as S
import           Monad
import           TH
import           Types


mailPattern :: ByteString
mailPattern = LB.init [litFile|src/mailPattern.txt|]

mailScraper :: Pipe ByteString ByteString IO ()
mailScraper = mailMatcher >-> validFilter >-> deduper

mailMatcher :: Pipe ByteString ByteString IO ()
mailMatcher = forever $ do
    content <- await
    let func = getAllTextMatches (content =~ mailPattern) :: [ByteString]
    mapM_ yield func

validFilter :: Pipe ByteString ByteString IO ()
validFilter = forever $ do
    m <- await
    let bs = SB.concat . LB.toChunks $ m
    when (isValid bs) $ yield m

deduper :: Pipe ByteString ByteString IO ()
deduper = evalStateP S.empty $ forever $ do
    bs <- await
    b <- gets (S.member bs)
    if b
    then return () -- already found
    else do
        modify $ insert bs
        yield bs











