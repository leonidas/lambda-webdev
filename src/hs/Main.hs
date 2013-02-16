{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsIntercept, settingsPort)

import Network.Wai.Application.Static (staticApp, defaultFileServerSettings, ssMaxAge)
import WaiAppStatic.Types (MaxAge(..))

import Network.Wai.Handler.WebSockets (intercept)
import qualified Network.WebSockets as WS

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Aeson (ToJSON(..), FromJSON(..), (.=))
import qualified Data.Aeson as JSON

import Network.WebSockets.Messaging

-- import Data.Text (Text)

import GHC.Generics (Generic)

import Control.Monad (void, liftM2, guard)

import Control.Concurrent (forkIO)

import Control.Concurrent.STM

data NameNotification = Name String deriving Generic

data ServerRequest
    = AskName
    | GameBoard Board
    deriving Generic

data Board = Board (Map Position Piece)

data Coord = Coord Int deriving (Eq, Ord)

type Position = (Coord, Coord)
data Piece = X | O

instance ToJSON Coord where
    toJSON (Coord i) = toJSON i

instance FromJSON Coord where
    parseJSON js = do
        i <- parseJSON js
        if (i >= 1 && i <= 3)
            then return $ Coord i
            else fail "invalid coordinate"

instance ToJSON Piece where
    toJSON X = JSON.String "X"
    toJSON O = JSON.String "O"

instance FromJSON Piece where
    parseJSON (JSON.String "X") = return X
    parseJSON (JSON.String "O") = return O
    parseJSON _ = fail "invalid Piece"

instance ToJSON Board where
    toJSON (Board mp) = toJSON $ Map.assocs mp

instance FromJSON Board where
    parseJSON js = do
        assocs <- parseJSON js
        return $ Board $ Map.fromList assocs

instance Message NameNotification
instance Message ServerRequest

app :: Application
app = staticApp $ (defaultFileServerSettings "public") { ssMaxAge = MaxAgeSeconds 1 }

initWSApp :: IO (WS.Request -> WS.WebSockets WS.Hybi00 ())
initWSApp = do

    queue <- newTChanIO

    void $ forkIO $ matchMaker queue

    return $ \req -> do
        WS.acceptRequest req
        onConnect $ \conn -> do
            atomically $ do
                notify conn $ GameBoard newBoard
                writeTChan queue conn

            return ()

newBoard :: Board
newBoard = Board $ Map.fromList
    [ ((Coord 1, Coord 1), X)
    , ((Coord 2, Coord 1), O)
    ]
-- newBoard = Board $ Map.empty

matchMaker :: TChan Connection -> IO ()
matchMaker queue = do
    (p1,p2) <- atomically $ liftM2 (,) (readTChan queue) (readTChan queue)
    return ()

nextConnected :: TChan Connection -> STM Connection
nextConnected queue = do
    conn <- readTChan queue
    disc <- readTVar $ disconnected conn
    if disc
        then nextConnected queue
        else return conn

main :: IO ()
main = do
    wsApp <- initWSApp

    let settings = defaultSettings
            { settingsIntercept = intercept wsApp
            , settingsPort      = 8000
            }

    runSettings settings app
