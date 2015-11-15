{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           TH

import           Control.Monad                (forM_, forever, when)
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.State          (StateT, evalStateT, get, modify,
                                               runStateT)
import qualified Data.ByteString              as SB
import           Data.ByteString.Lazy         (ByteString)
import qualified Data.ByteString.Lazy         as BS
import qualified Data.ByteString.Lazy.Char8   as BSLC
import           Network.HTTP.Client          (Manager (..), Request (..),
                                               closeManager, httpLbs,
                                               newManager, parseUrl,
                                               responseBody)
import           Network.HTTP.Client.Internal (setUriRelative)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.URI                  (parseURIReference)
import           Pipes
import           Pipes.Lift                   (evalStateP, runReaderP)
import           Pipes.Prelude                (tee)
import           Text.Email.Validate          (isValid)
import           Text.Regex.Posix             (getAllTextMatches, (=~))

crawlProducer :: Producer (Request, ByteString) Spider ()
crawlProducer = forever $ do
    list <- get
    when (not . null $ list) $ do
        req <- head `fmap` get
        manager <- ask

        body <- liftIO $ responseBody `fmap` httpLbs req manager
        yield (req, body)

urlPattern :: ByteString
urlPattern = BS.init [litFile|urlPattern.txt|]

addRequest :: Request -> Pipe a b Spider ()
addRequest req = modify ((:) req)

getUrls :: Pipe (Request, ByteString) ByteString Spider ()
getUrls = forever $ do
    (r, s) <- await
    let ms = getAllTextMatches (s =~ urlPattern) :: [ByteString]
    forM_ ms $ \u -> do
        let muri = parseURIReference . BSLC.unpack $ u
        case muri of
            Nothing -> return ()
            Just uri -> do
                req <- liftIO $ setUriRelative r uri
                addRequest req
    yield s

mailPattern :: ByteString
mailPattern = BS.init [litFile|mailPattern.txt|]

emailMatcher :: Pipe ByteString ByteString Spider ()
emailMatcher = forever $ do
    s <- await
    let func = getAllTextMatches (s =~ mailPattern) :: [ByteString]
    mapM_ yield func

validFilter :: Pipe ByteString ByteString Spider ()
validFilter = forever $ do
    m <- await
    let bs = SB.concat . BS.toChunks $ m
    when (isValid bs) $ yield m


printer :: Consumer ByteString Spider ()
printer = forever $ do
    bs <- await
    liftIO $ BS.putStr bs
    liftIO $ BS.putStr "\n"

type Spider = StateT State (ReaderT Manager IO)

type State = [Request]

main :: IO ()
main = do
    req <- liftIO $ parseUrl "http://www.gnu.org/software/mailman/mailman-admin/node4.html"

    let initState = [req]
    manager <- newManager tlsManagerSettings
    runReaderT (runStateT (runEffect $ crawlProducer >-> getUrls >-> emailMatcher >-> validFilter >-> printer) initState) manager
    return ()
