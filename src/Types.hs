{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Types
    ( module Types
    , module X
    ) where

import Control.Concurrent as X (threadDelay)
import Control.Concurrent.Async as X (Async(..), async, wait)
import Control.Concurrent.STM as X
       (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVar,
        writeTVar)
import Control.Monad as X
       (forM, forM_, forever, unless, void, when)
import Control.Monad.Reader as X (ReaderT, ask, asks, runReaderT)
import Control.Monad.State as X
       (StateT, evalStateT, get, gets, modify)
import Data.List as X (find)
import Data.Set as X (Set, deleteFindMin, insert, member, toList)
import Data.Time.Clock.POSIX as X (POSIXTime)
import Network.HTTP.Client as X
       (HttpException(..), Manager, Request(..), Response(..), httpLbs,
        httpNoBody, newManager, parseUrlThrow, responseBody,
        responseStatus)
import Network.HTTP.Client.Internal as X (setUriRelative)
import Network.HTTP.Client.TLS as X (tlsManagerSettings)
import Network.HTTP.Types.Header as X (hContentType)
import Network.HTTP.Types.Status as X (statusCode)
import Network.URI as X
       (URI(..), URIAuth(..), isRelativeReference, isURI,
        nonStrictRelativeTo, parseAbsoluteURI, parseRelativeReference,
        parseURI, parseURIReference, relativeTo)
import Pipes as X
       (Consumer, Pipe, Producer, Proxy, await, cat, liftIO, runEffect,
        yield, (>->))
import Pipes.Concurrent as X
       (bounded, forkIO, fromInput, performGC, spawn, toOutput, unbounded)
import Pipes.Lift as X (evalStateP)
import Pipes.Prelude as X (tee)
import Text.Email.Validate as X (isValid)
import Text.Regex.PCRE as X (getAllTextMatches, (=~))

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import Data.Ord (comparing)

instance Eq Request where
    e1 == e2 = getUri e1 == getUri e2

instance Ord Request where
    compare = comparing getUri

getUri :: Request -> String
getUri req = SBC.unpack $ SB.concat [prot, host req, path req]
  where
    prot =
        if secure req
            then "https://"
            else "http://"
