{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}


import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsIntercept, settingsPort)

import Network.Wai.Application.Static (staticApp, defaultFileServerSettings, ssMaxAge)
import WaiAppStatic.Types (MaxAge(..))

import Network.Wai.Handler.WebSockets (intercept)
import qualified Network.WebSockets as WS

import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as JSON

import Network.WebSockets.Messaging

-- import Data.Text (Text)

import GHC.Generics (Generic)

import Control.Applicative ((<|>),(<$>))
import Control.Monad (void, liftM2, guard, join, when)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Unsafe.Coerce

data NameNotification = Name String deriving Generic

data ServerRequest
    = AskName
    | AskMove
    | AskNewGame
    | FoundOpponent String
    | GameBoard Board
    | WonGame
    | LostGame
    | DrawGame
    deriving Generic

data Board = Board (Map Position Piece)

data Coord = Coord Int deriving (Eq, Ord)

type Position = (Coord, Coord)
data Piece = X | O

type family Other (p :: Piece) :: Piece
type instance Other X = O
type instance Other O = X

newtype Move (piece :: Piece) = Move Position deriving (FromJSON, ToJSON)

class MoveAssoc m where
    moveAssoc :: m -> (Position, Piece)

instance MoveAssoc (Move X) where
    moveAssoc (Move pos) = (pos, X)

instance MoveAssoc (Move O) where
    moveAssoc (Move pos) = (pos, O)

data User (piece :: Maybe Piece) = User
    { userName :: String
    , userConn :: Connection
    }

data Game turn = Game
    { gameBoard :: Board
    , gameState :: GameState turn
    }

data GameState (turn :: Piece) where
    Turn  :: (Move turn -> Maybe (Game (Other turn))) -> GameState turn
    Draw  :: GameState a
    Win   :: Piece -> GameState a

type NewPlayer    = User Nothing
type Player piece = User (Just piece)

foldGameState
    :: ((Move turn -> Maybe (Game (Other turn))) -> r)
    -> r
    -> (Piece -> r)
    -> GameState turn
    -> r
foldGameState handleTurn handleDraw handleWin s = case s of
    Turn f -> handleTurn f
    Draw   -> handleDraw
    Win p  -> handleWin p

getMove :: Player piece -> IO (Future (Move piece))
getMove (User{..}) = requestAsync userConn AskMove

assignSides :: NewPlayer -> NewPlayer -> (Player X, Player O)
assignSides pl1 pl2 = (unsafeCoerce pl1, unsafeCoerce pl2)

stripSide :: Player t -> NewPlayer
stripSide = unsafeCoerce

type Symmetric a b = (a ~ Other b, b ~ Other a)

type SymmetricMove turn =
    ( Other (Other turn) ~ turn
    , MoveAssoc (Move turn)
    , MoveAssoc (Move (Other turn))
    )

newGame :: Game X
newGame = Game newBoard $ Turn $ go newBoard where

    go :: SymmetricMove turn => Board -> Move turn -> Maybe (Game (Other turn))
    go (Board mp) move@(Move pos)
        | pos `Map.member` mp = Nothing
        | otherwise = Just $ Game board' $ fromMaybe (Turn $ go board') gameOver where
            board' = Board $ uncurry Map.insert (moveAssoc move) mp
            gameOver = Nothing


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
    toJSON (Board mp) = toJSON $
        [ [Map.lookup (Coord r, Coord c) mp| r <- [1..3]]
        | c <- [1..3]
        ]

instance FromJSON Board where
    parseJSON js = do
        rows <- parseJSON js
        let assocs = do
                (r, row) <- zip [1..3] rows
                (c, piece) <- zip [1..3] row
                case piece of
                    Just p -> return ((Coord r, Coord c), p)
                    _      -> []
        return $ Board $ Map.fromList assocs

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
            name <- request conn AskName
            atomically $ do
                notify conn $ GameBoard newBoard
                writeTChan queue $ User name conn

            return ()

newBoard :: Board
newBoard = Board $ Map.fromList
    [ ((Coord 1, Coord 1), X)
    , ((Coord 2, Coord 1), O)
    ]
-- newBoard = Board $ Map.empty

matchMaker :: TChan NewPlayer -> IO ()
matchMaker queue = do
    (p1,p2) <- atomically $ liftM2 (,) (readTChan queue) (readTChan queue)
    void $ forkIO $ playGame queue $ assignSides p1 p2

nextConnected :: TChan NewPlayer -> STM NewPlayer
nextConnected queue = do
    u@User{..} <- readTChan queue
    disc <- readTVar $ disconnected userConn
    if disc
        then nextConnected queue
        else return u

playGame :: TChan NewPlayer -> (Player X, Player O) -> IO ()
playGame queue (px, po) = start >> play >> both requeue where

    start = atomically $ do
        notify (userConn px) $ FoundOpponent $ userName po
        notify (userConn po) $ FoundOpponent $ userName px

    play = go px po newGame

    go :: Symmetric t t' => Player t -> Player t' -> Game t -> IO ()
    go p p' (Game b t) = sendBoard >> foldGameState turn draw win t where
        turn f = loop where
            loop        = getMove p >>= resolveMove
            resolveMove = join . atomically . foldFuture disco nextTurn
            disco       = atomically $ notify (userConn p') WonGame
            nextTurn m  = maybe loop (go p' p) (f m)

        draw = atomically $ both $ \p -> notify (userConn p) DrawGame

        win _ = atomically $ do
            notify (userConn p) LostGame
            notify (userConn p') WonGame

        sendBoard = atomically $ both $ \p -> notify (userConn p)  (GameBoard b)

    both :: Monad m => (forall t.Player t -> m ()) -> m ()
    both op = op px >> op po

    requeue :: Player t -> IO ()
    requeue p = void $ forkIO $ do
        yes <- request (userConn p) AskNewGame
        when yes $ atomically $ writeTChan queue $ stripSide p

main :: IO ()
main = do
    wsApp <- initWSApp

    let settings = defaultSettings
            { settingsIntercept = intercept wsApp
            , settingsPort      = 8000
            }

    runSettings settings app
