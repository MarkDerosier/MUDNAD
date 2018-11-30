{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Game ( engine
            ) where

-- Cloud Haskell
import qualified Control.Distributed.Process as CH
import Control.Distributed.Process.Internal.Types (ProcessId(..))
--

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Vector as VEC

import Control.Monad

import Datatypes

type World' = VEC.Vector (VEC.Vector Int)

engine :: ProcessId -> CH.Process ()
engine processid = do
    CH.send processid =<< CH.getSelfPid
    forever $ do
        (player, msg) <- CH.expect :: CH.Process (PlayerDatabaseID, T.Text)
        CH.liftIO $ T.putStrLn msg



parseInput :: (PlayerID, T.Text) -> Either String PlayerCommand
parseInput (player, msg)
    | msg == "add" = Right INC
    | msg == "sub" = Right DEINC
    | otherwise    = Left "failed to parse"


data PlayerCommand = 
      INC
    | DEINC
  

type World = Int

evalWorld :: (PlayerID, T.Text) -> World -> World
evalWorld (player, msg) world = do
    let command = parseInput (player, msg)
    case command of
        Right INC   -> world + 1
        Right DEINC -> world - 1
