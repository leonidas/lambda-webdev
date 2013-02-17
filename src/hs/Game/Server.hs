{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Server (initWSApp, app) where

import Network.Wai (Application)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings, ssMaxAge)
import WaiAppStatic.Types (MaxAge(..))
import qualified Network.WebSockets as WS

import Control.Concurrent.STM (STM, newTChanIO, atomically, writeTChan, TChan, readTChan, readTVar)
import Control.Concurrent (forkIO)
import Control.Monad (void, liftM2)

import Network.WebSockets.Messaging (onConnect, request, notify, disconnected)

import Game.Protocol (ServerRequest(..))
import Game.Types (User(..))
import Game.Logic (playGame, assignSides)

type NewPlayer    = User Nothing
type Player piece = User (Just piece)

app :: Application
app = staticApp $ (defaultFileServerSettings "public")
    { ssMaxAge = MaxAgeSeconds 1 }

initWSApp :: IO (WS.Request -> WS.WebSockets WS.Hybi00 ())
initWSApp = do

    queue <- newTChanIO

    void $ forkIO $ matchMaker queue

    return $ \req -> do
        WS.acceptRequest req
        onConnect $ \conn -> do
            name <- request conn AskName
            atomically $ do
                writeTChan queue $ User name conn

            return ()

matchMaker :: TChan NewPlayer -> IO ()
matchMaker queue = do
    (p1,p2) <- atomically $ liftM2 (,) (nextConnected queue) (nextConnected queue)
    void $ forkIO $ playGame queue $ assignSides p1 p2

nextConnected :: TChan NewPlayer -> STM NewPlayer
nextConnected queue = do
    u@User{..} <- readTChan queue
    disc <- readTVar $ disconnected userConn
    if disc
        then nextConnected queue
        else return u

