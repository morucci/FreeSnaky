{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Options.Generic
import Relude
import Server
import Tui

data Args w
  = Client
      { address :: w ::: Text <?> "Address to connect",
        port :: w ::: Int <?> "Port to connect" <!> "9160",
        snakeName :: w ::: Text <?> "Snake name" <!> "FreeSnaky"
      }
  | Server
      { bindAddress :: w ::: Text <?> "Address to bind to",
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
    Client addr port name -> runClient addr port name
    Server addr port -> runServer addr port Nothing
    Local name ->
      withAsync (runServerLocal (Just . const $ pure ())) $
        \_ -> do
          threadDelay 1000000
          runClientLocal name
