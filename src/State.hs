module State where

import           Types

data State = State {
          queue   :: BankersDequeue Request
        , manager :: Manager
    }
