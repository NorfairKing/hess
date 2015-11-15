module Types (
          module Types
        , module Network.HTTP.Client
        , module Data.Dequeue
        , module Network.URI
        , module Pipes
        , module Control.Monad.State
        , module Control.Monad.Reader
        , module Control.Monad
        , module Text.Regex.Posix
        , module Text.Email.Validate
        , module Network.HTTP.Client.Internal
        , module Data.Set
        , module Network.HTTP.Client.TLS
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
                                               responseBody)
import           Network.HTTP.Client.Internal (setUriRelative)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.URI                  (URI (..), parseURI,
                                               parseURIReference)
import           Pipes                        (Consumer (..), Pipe (..),
                                               Producer (..), Proxy (..), await,
                                               liftIO, runEffect, yield, (>->))
import           Text.Email.Validate          (isValid)
import           Text.Regex.Posix             (getAllTextMatches, (=~))


