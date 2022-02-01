module Client where

import Control.Concurrent (forkIO)
import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Relude
import Server (ProtoMessages (Hello))

app :: WS.ClientApp ()
app conn = do
  putStrLn "Connected!"

  -- Fork a thread that writes WS data to stdout
  _ <- forkIO $
    forever $ do
      msg <- WS.receiveData conn
      liftIO $ T.putStrLn msg

  -- Read from stdin and write to WS
  let loop = do
        line <- T.getLine
        unless (T.null line) $ do
          WS.sendTextData conn line
          loop

  loop
  WS.sendClose conn ("Bye!" :: Text)

main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app

-------

app2 :: WS.ClientApp ()
app2 conn = do
  putStrLn "Connected!"

  WS.sendTextData conn $ encode (Hello "Hi ! I am Fabien")

main2 :: IO ()
main2 = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app2
