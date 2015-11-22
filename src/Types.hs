{-# LANGUAGE OverloadedStrings #-}
module Types (
          module Types
        , module Control.Concurrent
        , module Control.Concurrent.Async
        , module Control.Concurrent.STM
        , module Control.Lens
        , module Control.Monad
        , module Control.Monad.Reader
        , module Control.Monad.State
        , module Data.Dequeue
        , module Data.List
        , module Data.Set
        , module Network.HTTP.Client
        , module Network.HTTP.Client.Internal
        , module Network.HTTP.Client.TLS
        , module Network.HTTP.Types.Header
        , module Network.HTTP.Types.Status
        , module Network.URI
        , module Pipes
        , module Pipes.Concurrent
        , module Text.Email.Validate
        , module Text.Regex.Posix
    ) where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, wait)
import           Control.Concurrent.STM       (TVar (..), atomically,
                                               modifyTVar', newTVarIO, readTVar,
                                               readTVar, writeTVar)
import           Control.Lens                 (makeLenses, use, uses, view,
                                               (%=), (%~), (&), (.=), (.~),
                                               (^.))
import           Control.Monad                (forM, forM_, forever, unless,
                                               void, when)
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.State          (StateT, evalStateT, get, gets,
                                               modify)
import qualified Data.ByteString              as SB
import qualified Data.ByteString.Char8        as SBC
import           Data.Dequeue                 (BankersDequeue (..),
                                               Dequeue (..))
import           Data.List                    (find)
import           Data.Ord                     (comparing)
import           Data.Set                     (Set (..), deleteFindMin, insert,
                                               member, toList)
import           Network.HTTP.Client          (HttpException (..), Manager (..),
                                               Request (..), Response (..),
                                               httpLbs, newManager, parseUrl,
                                               responseBody, responseStatus)
import           Network.HTTP.Client.Internal (setUriRelative)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Types.Header    (hContentType)
import           Network.HTTP.Types.Status    (statusCode)
import           Network.URI                  (URI (..), parseURI,
                                               parseURIReference)
import           Pipes                        (Consumer (..), Pipe (..),
                                               Producer (..), Proxy (..), await,
                                               liftIO, runEffect, yield, (>->))
import           Pipes.Concurrent             (bounded, forkIO, fromInput,
                                               performGC, spawn, toOutput)
import           Text.Email.Validate          (isValid)
import           Text.Regex.Posix             (getAllTextMatches, (=~))

instance Eq Request where
    e1 == e2 = getUri e1 == getUri e2

instance Ord Request where
    compare = comparing getUri

getUri :: Request -> String
getUri req = SBC.unpack $ SB.concat [prot, host req, path req]
    where prot = if secure req then "https://" else "http://"

