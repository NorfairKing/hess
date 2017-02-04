module Monad where

import Types

{-# ANN module "HLint: ignore Use camelCase" #-}

data Args = Args
    { seed_uri :: URI
    , nr_fetchers :: Int
    , fetchers_status_logging :: Bool
    , fetchers_status_logging_file :: FilePath
    , mail_store :: FilePath
    , on_queue_logging :: Bool
    , on_queue_log_file :: FilePath
    , stay_within_domain :: Bool
    } deriving (Show, Eq)

type HESS = ReaderT Args IO
