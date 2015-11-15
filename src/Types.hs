module Types (
          module Types
        , module Control.Monad
        , module Control.Monad.Reader
        , module Control.Monad.State
        , module Data.Dequeue
        , module Data.Set
        , module Network.HTTP.Client
        , module Network.HTTP.Client.Internal
        , module Network.HTTP.Client.TLS
        , module Network.HTTP.Types.Status
        , module Network.URI
        , module Pipes
        , module Text.Email.Validate
        , module Text.Regex.Posix
    ) where

import           Control.Monad                (forM_, forever, when)
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.State          (StateT, evalStateT, get, gets,
                                               modify)
import           Data.Dequeue                 (BankersDequeue (..),
                                               Dequeue (..))
import           Data.Set                     (Set (..), insert, member)
import           Network.HTTP.Client          (Manager (..), Request (..),
                                               httpLbs, newManager, parseUrl,
                                               responseBody, responseStatus)
import           Network.HTTP.Client.Internal (setUriRelative)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Types.Status    (statusCode)
import           Network.URI                  (URI (..), parseURI,
                                               parseURIReference)
import           Pipes                        (Consumer (..), Pipe (..),
                                               Producer (..), Proxy (..), await,
                                               liftIO, runEffect, yield, (>->))
import           Text.Email.Validate          (isValid)
import           Text.Regex.Posix             (getAllTextMatches, (=~))


