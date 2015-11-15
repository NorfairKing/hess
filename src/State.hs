module State where

import           Types

data State = State {
          queue   :: Set Request
        , visited :: Set String
        , manager :: Manager
    }
