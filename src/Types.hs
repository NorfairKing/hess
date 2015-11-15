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
        , module Network.HTTP.Client.TLS
    ) where

import           Control.Monad                (forM_, forever, when)
import           Control.Monad.Reader         (ReaderT, ask, runReaderT)
import           Control.Monad.State          (StateT, evalStateT, get, gets,
                                               modify)
import           Data.Dequeue                 (BankersDequeue (..),
                                               Dequeue (..))
import           Network.HTTP.Client          (Manager (..), Request (..),
                                               newManager, parseUrl)
import           Network.HTTP.Client.Internal (setUriRelative)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.URI                  (URI (..), parseURIReference)
import           Pipes                        (Consumer (..), Pipe (..),
                                               Producer (..), await, liftIO,
                                               runEffect, yield, (>->))
import           Text.Email.Validate          (isValid)
import           Text.Regex.Posix             (getAllTextMatches, (=~))


