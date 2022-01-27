module Server where

import qualified Network.WebSockets as WS
import Relude

type Client = (Text, WS.Connection)

type ServerState = [Client]

newserverState :: [ServerState]
newserverState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)
