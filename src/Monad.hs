module Monad where

import           Types

data Args = Args {
        _nr_fetchers :: Int
    } deriving (Show, Eq)

makeLenses ''Args


type HESS = ReaderT Args IO

