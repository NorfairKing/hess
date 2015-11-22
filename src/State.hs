module State where

import           Types

data State = State {
          _queue   :: Set Request
        , _visited :: Set String
        , _manager :: Manager
    }

makeLenses ''State
