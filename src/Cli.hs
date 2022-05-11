{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Cli (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Options.Generic
import Server
import System.Log.FastLogger
  ( LogType' (LogStdout),
    defaultBufSize,
  )
import Tui
import Prelude

data Args w
  = Client
      { address :: w ::: String <?> "Address to connect",
        port :: w ::: Int <?> "Port to connect" <!> "9160",
        snakeName :: w ::: Text <?> "Snake name" <!> "FreeSnaky"
      }
  | Server
      { bindAddress :: w ::: String <?> "Address to bind to",
        bindPort :: w ::: Int <?> "Port to bind to" <!> "9160"
      }
  | Local
      {snakeName :: w ::: Text <?> "Snake name" <!> "FreeSnaky"}
  deriving (Generic)

instance ParseRecord (Args Wrapped)

deriving instance Show (Args Unwrapped)

main :: IO ()
main = do
  args <- unwrapRecord "Free Snaky"
  case (args :: Args Unwrapped) of
    Client addr port name -> runClient (NetworkAddr addr port) name
    Server addr port -> runServer (NetworkAddr addr port) (LogStdout defaultBufSize)
    Local name ->
      withAsync runServerLocal $
        \_ -> do
          threadDelay 1000000
          runClientLocal name
