{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE UnicodeSyntax      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

module Main where

-- Cloud Haskell
import qualified Control.Distributed.Process as CH 
import Control.Distributed.Process.Node
import qualified Control.Distributed.Process.Backend.SimpleLocalnet as SL
import Control.Distributed.Process.Internal.Types (NodeId(..), ProcessId(..), LocalProcessId(..))
import Data.Int (Int32)
import Network.Transport (EndPointAddress(..))
--

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Data.Atomics.Counter

import qualified Data.Text as T
import qualified Data.ByteString as BS hiding (pack)
import qualified Data.ByteString.Char8 as BS

import Network.WebSockets as WS
import Control.Exception

-- Redis Database connecton
import qualified Database.Redis as RS
--

import Control.Applicative
import Data.Semigroup

import System.Environment (getArgs)


-- Personal
import Storage
import Datatypes
import qualified Game
import qualified Lobby



data LobbyPath = CreateAccount | LoginAccount deriving (Show)
data LoginState = LoginState (Maybe (T.Text, Maybe T.Text)) deriving (Show)


--routeMsg :: ProcessId -> MAP.Map PlayerID ProcessId -> ServerMsg

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["lobby", host, port] -> do
            backend <- SL.initializeBackend host port initRemoteTable
            node    <- SL.newLocalNode backend
            print $ localNodeId node
            pid     <- forkProcess node Lobby.lobby
            --get primatives to construct LocalnodeId on server
            case pid of
                ProcessId _ (LocalProcessId pidcounter pidid) -> do 
                    putStrLn ( (show pidcounter) ++ " " ++  (show pidid))

            redis   <- RS.checkedConnect RS.defaultConnectInfo
            counter <- newCounter 0
            runServer "localhost" 4444 (connectionHandler pid node redis counter)

        ["game", host, port, lobbyAddress, pidcounter, pidid] -> do
            backend <- SL.initializeBackend host port initRemoteTable
            node <- SL.newLocalNode backend
            let nodeid = NodeId . EndPointAddress $ BS.pack lobbyAddress
            let localprocess = LocalProcessId (read pidcounter :: Int32) (read pidid :: Int32)
            let processid = ProcessId nodeid localprocess
            runProcess node $ Game.engine processid

        _ -> putStrLn $ 
                "Format:\n" <> 
                "MUDNAD-exe lobby ip port\n" <>
                "MUDNAD-exe game ip port ip:8888:0 number number"


connectionHandler :: ProcessId -> LocalNode -> RS.Connection -> AtomicCounter -> ServerApp
connectionHandler lobby node redis counter pending = do 
    playercount <- readCounter counter
    print playercount
    if playercount < 4
        then do connected <- acceptRequest pending
                incrCounter_ 1 counter
                banner <- getBanner redis
                maybe empty (sendTextData connected) banner

                loginSession <- async $ loginLoop (LoginState Nothing) redis connected
                timeout <- async $ threadDelay 10000000
                result  <- waitEitherCatch timeout loginSession
                print result
                incrCounter_ (-1) counter

                case result of
                    Right (Right player@(PlayerID _ databaseid)) -> do
                        finally
                            (runProcess node $ inputProcess node lobby player connected)
                            (runProcess node $ CH.send lobby (Logout databaseid))
                    Right (Left  _)      -> WS.sendClose connected ("Timeout for inactivity" :: T.Text)
                    Left _               -> WS.sendClose connected ("timeout for inactivity" :: T.Text) >> cancel loginSession

        else do connected <- acceptRequest pending
                sendClose connected ("Lobby is full!" :: T.Text)


loginLoop :: LoginState -> RS.Connection -> WS.Connection -> IO PlayerID
loginLoop (LoginState Nothing) redis conn = do 
    sendTextData conn ("Username:\n" :: T.Text)
    username <- WS.fromDataMessage <$> receiveDataMessage conn :: IO T.Text
    loginLoop (LoginState (Just (username, Nothing))) redis conn

loginLoop (LoginState (Just (username, Nothing))) redis conn = do
    sendTextData conn ("Password:\n" :: T.Text)
    password <- WS.fromDataMessage <$> receiveDataMessage conn :: IO T.Text
    loginLoop (LoginState (Just (username, Just password))) redis conn

loginLoop (LoginState (Just (username, Just password))) redis conn = do
    result <- login redis username password
    case result of
        Just userid -> return $ PlayerID (PlayerName username) userid
        Nothing     -> do sendTextData conn ("Login failed.\n" :: T.Text)
                          loginLoop (LoginState Nothing) redis conn



inputProcess :: LocalNode -> ProcessId -> PlayerID -> WS.Connection -> CH.Process ()
inputProcess node lobby player@(PlayerID _ databaseid) conn = do
    pid <- CH.getSelfPid
    CH.send lobby $ Lobby.Login (player, pid)
    didlogin <- CH.expect :: CH.Process Bool
    case didlogin of
        False -> CH.liftIO $ WS.sendClose conn ("You are already logged in!" :: T.Text)
        True  -> do
            CH.link =<< (CH.liftIO $ forkProcess node $ clientInput lobby pid databaseid conn)
            forever $ do
                msg <- CH.expect :: CH.Process T.Text
                CH.liftIO $ sendTextData conn msg

clientInput :: ProcessId -> ProcessId -> PlayerDatabaseID -> WS.Connection -> CH.Process ()
clientInput lobby screenprocess player conn = forever $ do 
    msg <- CH.liftIO $ WS.fromDataMessage <$> receiveDataMessage conn :: CH.Process T.Text
    CH.send screenprocess msg
    CH.send lobby (player, msg)
