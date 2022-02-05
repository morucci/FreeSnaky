module Client where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Data.Aeson (encode)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Relude
import Say (say)
import Server (ProtoMessages (Bye, Hello, SnakeDirection, Tick, Welcome), getProtoMessage)
import Snake (Direction (RIGHT))

logText :: Text -> IO ()
logText = say

app :: WS.ClientApp ()
app conn = do
  putStrLn "WS Connected!"

  let clientId = "fakeid"

  WS.sendTextData conn $ encode (Hello clientId)

  void $
    forever $ do
      resp <- getProtoMessage conn
      case resp of
        Welcome -> do
          logText "Connected!"
          handleFakeClient
        Bye -> do
          logText "Same clientId already connected!"
          pure ()
        _ -> print ("Not Implemented" :: Text)
  where
    handleFakeClient :: IO ()
    handleFakeClient = do
      concurrently_ sendCommands readMessages
      where
        sendCommands :: IO ()
        sendCommands = do
          threadDelay 1000000
          WS.sendTextData conn $ encode (SnakeDirection RIGHT)
          sendCommands
        readMessages :: IO ()
        readMessages = do
          resp <- getProtoMessage conn
          case resp of
            Tick _ -> do
              logText "Received Tick"
            _ -> logText "Not implemented"
          readMessages

main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app
