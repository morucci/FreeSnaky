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
    eitherDecodeFileStrict,
    encodeFile,
  )
import qualified Data.Text as T
import Data.Time
import GHC.Generics (Generic)
import System.Directory
  ( XdgDirectory (XdgData),
    createDirectoryIfMissing,
    doesFileExist,
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

newBoard :: Board
newBoard = Board []

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

loadBoard :: IO (Either String Board)
loadBoard = do
  path <- dumpPath
  fileExists <- doesFileExist path
  if not fileExists
    then pure $ Right newBoard
    else eitherDecodeFileStrict path

addScore :: Ident -> Score -> UTCTime -> Board -> Board
addScore ident score now (Board entries) = Board $ entries <> [BoardEntry ident score now]

-- Main functions
-----------------

-- | Load the board from the disk and return a LeaderBoard type
loadLeaderBoard :: IO (Either String LeaderBoard)
loadLeaderBoard = do
  boardE <- loadBoard
  case boardE of
    Left err -> pure $ Left err
    Right board -> do
      mvar <- newMVar board
      pure . Right $ LeaderBoard mvar

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
        Right boardUpdated -> (boardUpdated, Right ())
