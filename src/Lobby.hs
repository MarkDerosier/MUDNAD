{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Lobby ( lobby
             , Login(..)
             ) where 


-- Cloud Haskell
import Control.Distributed.Process
-- For deriving Serializable for Cloud Haskell
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
--

import qualified Data.Map.Strict as MAP

import qualified Data.Text as T

-- Personal
import Datatypes

data Login = Login (PlayerID, ProcessId) deriving (Show, Generic, Typeable, Eq, Ord)
instance Binary Login

type Players = MAP.Map PlayerDatabaseID (PlayerName, ProcessId)

lobby :: Process ()
lobby = do
    game  <- receiveWait [match (\x -> return x :: Process ProcessId)]
    lobbyLoop game (MAP.empty)



lobbyLoop :: ProcessId -> Players -> Process ()
lobbyLoop game playerlist = do
    (newgame, newlist) <- receiveWait messages
    lobbyLoop newgame newlist

    where messages = [
             match $ forwardMsg   game playerlist
           , match $ switchServer game playerlist
           , match $ logoutPlayer game playerlist
           , match $ loginAttempt game playerlist
           ]

-- Forwards player input from the lobby to the game engine
forwardMsg :: ProcessId ->
              Players   ->
              (PlayerDatabaseID, T.Text) ->
              Process (ProcessId, Players)
forwardMsg game playerlist (player, msg) = do
    send game (player, msg)
    return (game, playerlist)


-- Switches to a new game engine once new game enigine
-- broadcasts itself
switchServer :: ProcessId ->
                Players   ->
                ProcessId ->
                Process (ProcessId, Players)
switchServer game playerlist newgame = return (newgame, playerlist)


logoutPlayer :: ProcessId ->
                Players   ->
                Logout    ->
                Process (ProcessId, Players)
logoutPlayer game playerlist (Logout player) = do
    let newplayerlist = MAP.delete player playerlist
    return (game, newplayerlist)



loginAttempt :: ProcessId ->
                Players   ->
                Login     ->
                Process (ProcessId, Players)
loginAttempt game playerlist (Login (PlayerID player databaseid, pid)) = do
    let  loggedin = MAP.member databaseid playerlist
    case loggedin of
        True  -> send pid False >> return (game, playerlist)
        False -> do
            let newplayerlist = MAP.insert databaseid (player, pid) playerlist
            send pid True
            return (game, newplayerlist)


