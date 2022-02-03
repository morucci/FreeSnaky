module Client where

import Data.Aeson (encode)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Relude
import Say (say)
import Server (ProtoMessages (Bye, Hello, Tick, Welcome), getProtoMessage)

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
          pure ()
        Bye -> do
          logText "Same clientId already connected!"
          pure ()
        Tick _ -> do
          logText "Received Tick"
        _ -> print ("Not Implemented" :: Text)

main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app
