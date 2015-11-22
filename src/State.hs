module State where

import           Types

data CrawlerState = CState {
          _visited :: TVar (Set URI)
        , _manager :: Manager
    }

makeLenses ''CrawlerState
