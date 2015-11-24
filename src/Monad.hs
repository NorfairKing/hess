{-# LANGUAGE TemplateHaskell #-}
module Monad where

import           Types

data Args = Args {
        _nr_fetchers :: Int
    ,   _mail_store  :: FilePath
    ,   _visited_log :: FilePath
    } deriving (Show, Eq)

makeLenses ''Args


type HESS = ReaderT Args IO

