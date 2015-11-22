module State where

import           Types

type State = TVar IState

data IState = IState {
          _queue   :: Set Request
        , _visited :: Set String
        , _manager :: Manager
    }

makeLenses ''IState
