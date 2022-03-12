-- |
-- Module      : LeaderBoard
-- Description : A Simple thread safe leader board
-- Copyright   : (c) Fabien Boucher, 2022
-- License     : MIT
-- Maintainer  : fabien.dot.boucher@gmail.com
--
-- This module contains a thread safe leader board. The board is loaded
-- and dumped on the disk in XDG data dir.
module LeaderBoard
  ( -- * Types
    LeaderBoard (..),
    Board (..),
    BoardEntry (..),

    -- * Functions to manage the LeaderBoard
    loadLeaderBoard,
    readLeaderBoard,
    writeLeaderBoard,
  )
where

import Control.Concurrent
  ( MVar,
    modifyMVar,
    newMVar,
    readMVar,
  )
import Control.Exception
  ( SomeException,
    try,
  )
import Data.Aeson
  ( FromJSON,
    ToJSON,
    decodeFileStrict,
    encodeFile,
  )
import qualified Data.Text as T
import Data.Time
import GHC.Generics (Generic)
import System.Directory
  ( XdgDirectory (XdgData),
    createDirectoryIfMissing,
    getXdgDirectory,
  )
import Prelude

type Ident = T.Text

type Score = Int

-- | An entry of the Board
data BoardEntry = BoardEntry
  { name :: Ident,
    score :: Score,
    date :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON BoardEntry

instance ToJSON BoardEntry

instance FromJSON Board

instance ToJSON Board

-- | A container that is a list of BoardEntry
newtype Board = Board [BoardEntry] deriving (Show, Generic)

-- | The LeaderBoard that is a MVar of Board
newtype LeaderBoard = LeaderBoard (MVar Board)

dumpPath :: IO FilePath
dumpPath = do
  dataDir <- getXdgDirectory XdgData "freesnaky"
  createDirectoryIfMissing True dataDir
  pure $ dataDir <> "/board.json"

dumpBoard :: Board -> IO (Either SomeException Board)
dumpBoard board = do
  path <- dumpPath
  ret <- try $ encodeFile path board
  pure $ case ret of
    Left exc -> Left exc
    Right _ -> Right board

loadBoard :: IO Board
loadBoard = do
  path <- dumpPath
  boardM <- load path
  pure $ case boardM of
    Nothing -> Board []
    Just board -> board
  where
    load :: FilePath -> IO (Maybe Board)
    load path = do
      boardME <- load' path
      pure $ case boardME of
        Left _ -> Just $ Board []
        Right boardM -> boardM
    load' :: FilePath -> IO (Either SomeException (Maybe Board))
    load' = try . decodeFileStrict

addScore :: Ident -> Score -> UTCTime -> Board -> Board
addScore ident score now (Board entries) = Board $ entries <> [BoardEntry ident score now]

-- Main functions
-----------------

-- | Load the board from the disk and return a LeaderBoard type
loadLeaderBoard :: IO LeaderBoard
loadLeaderBoard = do
  mvar <- loadBoard >>= newMVar
  pure $ LeaderBoard mvar

-- | Read the LeaderBoard (extract of the MVar) to return the Board
readLeaderBoard :: LeaderBoard -> IO Board
readLeaderBoard (LeaderBoard mvar) = readMVar mvar

-- | Write the LeaderBoard by adding or not a new Score
writeLeaderBoard :: LeaderBoard -> Maybe (Ident, Score) -> IO (Either SomeException ())
writeLeaderBoard (LeaderBoard mvar) entryM = modifyMVar mvar add
  where
    add board = do
      dumpStatus <- case entryM of
        Nothing -> dumpBoard board
        Just (ident, score) -> do
          now <- getCurrentTime
          dumpBoard $ addScore ident score now board
      pure $ case dumpStatus of
        Left exc -> (board, Left exc)
        Right newBoard -> (newBoard, Right ())
