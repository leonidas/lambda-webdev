{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

module Game.Server (initWSApp, app) where

import Network.Wai (Application)
import Network.Wai.Application.Static
    ( staticApp
    , defaultFileServerSettings
    , ssMaxAge
    )

import WaiAppStatic.Types (MaxAge(..))
import qualified Network.WebSockets as WS

import Control.Concurrent.STM
    ( STM
    , newTChanIO
    , atomically
    , writeTChan
    , TChan
    , readTChan
    , readTVar
    )

import Control.Concurrent (forkIO)
import Control.Monad (void, liftM2, forever, join, when)

import Network.WebSockets.Messaging
    ( onConnect
    , request
    , notify
    , disconnected
    , foldFuture
    )

import Game.Protocol (ServerRequest(..), GameResult(..))
import Game.Logic (newGame, Game(..), foldGameStatus)
import Game.Move (requestMove)
import Game.Types

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

matchMaker :: TChan NewPlayer -> IO ()
matchMaker queue = forever $ do
    (p1,p2) <- atomically $ liftM2 (,) (nextConnected queue) (nextConnected queue)
    void $ forkIO $ playGame queue $ assignSides p1 p2

nextConnected :: TChan NewPlayer -> STM NewPlayer
nextConnected queue = do
    u@User{..} <- readTChan queue
    disc <- readTVar $ disconnected userConn
    if disc
        then nextConnected queue
        else return u

type Cyclic a b = (a ~ Other b, b ~ Other a)

playGame :: TChan NewPlayer -> (Player X, Player O) -> IO ()
playGame queue (px, po) = start >> play >> both requeue where

    start = atomically $ do
        notify (userConn px) $ FoundOpponent $ userName po
        notify (userConn po) $ FoundOpponent $ userName px

    play = go px po newGame

    go :: Cyclic t t' => Player t -> Player t' -> Game t -> IO ()
    go p p' (Game b st) = sendBoard >> foldGameStatus turn draw win st where
        turn f = loop where
            loop        = requestMove p >>= resolveMove
            resolveMove = join . atomically . foldFuture disconnect nextTurn
            disconnect  = atomically $ notifyResult p' WonGame
            nextTurn m  = maybe loop (go p' p) (f m)

        draw = atomically $ both $ \u -> notifyResult u DrawGame

        win _ = atomically $ do
            notifyResult p  LostGame
            notifyResult p' WonGame

        sendBoard = atomically $ both $ \u -> notify (userConn u) (GameBoard b)

    notifyResult u = notify (userConn u) . GameOver

    both :: Monad m => (forall t.Player t -> m ()) -> m ()
    both op = op px >> op po

    requeue :: Player t -> IO ()
    requeue p = void $ forkIO $ do
        yes <- request (userConn p) AskNewGame
        when yes $ atomically $ writeTChan queue $ stripSide p
