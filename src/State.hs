module State where

import           Types

data State = State {
          queue   :: BankersDequeue Request
        , visited :: Set URI
        , manager :: Manager
    }
