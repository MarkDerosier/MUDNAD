{-# LANGUAGE DeriveGeneric      #-}

module Datatypes ( PlayerID(..)
                 , Logout(..)
                 , PlayerMsg(..)
                 , PlayerName(..)
                 , PlayerDatabaseID(..)
                 , BlockID(..)
                 ) where

import qualified Data.Text as T

-- For deriving Serializable for Cloud Haskell
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Data.Hashable
--

import qualified Data.ByteString as BS hiding (pack)

-- For storing voxels
import Data.Vector

newtype PlayerName = PlayerName T.Text deriving (Show, Generic, Typeable, Eq, Ord)
instance Hashable PlayerName
instance Binary   PlayerName

newtype PlayerDatabaseID = PlayerDatabaseID BS.ByteString deriving (Show, Generic, Typeable, Eq, Ord)
instance Hashable PlayerDatabaseID
instance Binary   PlayerDatabaseID

-- Type that contains the players displayed name, and the database id
-- The id never changes, but the player name can be.
-- Use the id internal in the world save
data PlayerID = PlayerID PlayerName PlayerDatabaseID deriving (Show, Generic, Typeable, Eq, Ord)
instance Binary PlayerID
instance Hashable PlayerID

newtype Logout = Logout PlayerDatabaseID deriving (Show, Generic, Typeable, Eq, Ord)
instance Binary Logout

-- Type for all messages sent from client to server
data PlayerMsg = PlayerMsg PlayerID T.Text deriving (Show, Generic, Typeable, Eq, Ord)
instance Binary PlayerMsg


-- Type for messages send from the game server to the lobby server
-- to be routed to players
data PlayerReply = PlayerReply PlayerID T.Text deriving (Show, Generic, Typeable, Eq, Ord)
instance Binary PlayerReply



data Color = 
      Red
    | Blue
    | Green
    | Orange
    | Black
    | Gray
    | Yellow
    deriving (Show)

type BackGroundColor = Color
type TextColor       = Color

data TypeFace =
      Italics
    | Bold
    | SmallCaps
    | Empathasis
    deriving (Show)

type Font = T.Text

data MudMessage = MudMessage T.Text TextColor BackGroundColor Font


-- Voxel game world

type Grid = Vector (Vector (Vector Int))

data BlockID = BlockID Int

data Chunk = Block | Grid
