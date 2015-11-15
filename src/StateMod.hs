module StateMod where

import           Monad
import           State
import           Types

addRequest :: Request -> Pipe a b Spider ()
addRequest req = do
    q <- gets queue
    let newq = pushBack q req
    modify (\s -> s { queue = newq })

