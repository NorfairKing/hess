module Monad where

import           State
import           Types

type Spider = StateT CrawlerState IO

