{-# LANGUAGE TemplateHaskell #-}
module Monad where

import           Types

data Args = Args {
        _seed_uri                     :: URI
    ,   _nr_fetchers                  :: Int
    ,   _fetchers_status_logging      :: Bool
    ,   _fetchers_status_logging_file :: FilePath
    ,   _mail_store                   :: FilePath
    ,   _on_queue_logging             :: Bool
    ,   _on_queue_log_file            :: FilePath
    ,   _stay_within_domain           :: Bool
    } deriving (Show, Eq)

makeLenses ''Args


type HESS = ReaderT Args IO

