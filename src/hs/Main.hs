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
{-# LANGUAGE RankNTypes #-}


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

import GHC.Generics (Generic)

import Control.Applicative ((<|>))
import Control.Monad (void, liftM2, join, when, msum)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Unsafe.Coerce

(<?>) :: Maybe a -> a -> a
(<?>) = flip fromMaybe

maybeIf :: Bool -> a -> Maybe a
maybeIf p a
    | p         = Just a
    | otherwise = Nothing

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
data Piece = X | O deriving (Show, Eq)

type family Other (p :: Piece) :: Piece
type instance Other X = O
type instance Other O = X

newtype Move (piece :: Piece) = Move Position deriving (FromJSON, ToJSON)

class MoveAssoc (p :: Piece) where
    moveAssoc :: Move p -> (Position, Piece)
    moveAssoc m@(Move pos) = (pos, movePiece m)

    movePiece :: Move p -> Piece

instance MoveAssoc X where
    movePiece _ = X

instance MoveAssoc O where
    movePiece _ = O

data User (piece :: Maybe Piece) = User
    { userName :: String
    , userConn :: Connection
    }

data Game turn = Game Board (GameStatus turn)

data GameStatus (turn :: Piece) where
    Turn  :: ProcessMove turn -> GameStatus turn
    Draw  :: GameStatus a
    Win   :: Piece -> GameStatus a

type NewPlayer    = User Nothing
type Player piece = User (Just piece)

type ProcessMove turn = Move turn -> Maybe (Game (Other turn))

foldGameState
    :: (ProcessMove turn -> r)
    -> r
    -> (Piece -> r)
    -> GameStatus turn
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

type Cyclic a b = (a ~ Other b, b ~ Other a)

type CyclicMove turn =
    ( Other (Other turn) ~ turn
    , MoveAssoc turn
    , MoveAssoc (Other turn)
    )

newGame :: Game X
newGame = Game newBoard $ Turn $ go newBoard where

    go :: CyclicMove turn => Board -> Move turn -> Maybe (Game (Other turn))
    go (Board mp) move
        | pos `Map.member` mp = Nothing
        | otherwise = Just $ Game board' $ gameOver <?> Turn (go board')
        where
            assoc@(pos, piece) = moveAssoc move
            board'   = Board mp'
            mp'      = uncurry Map.insert assoc mp
            gameOver = victory <|> draw
            draw     = maybeIf (Map.size mp' == 9) Draw
            victory  = fullRow <|> fullCol <|> fullDiag
            fullRow  = msum [match (1, r) (1, 0) | r <- [1..3]]
            fullCol  = msum [match (c, 1) (0, 1) | c <- [1..3]]
            fullDiag = match (1, 1) (1, 1) <|> match (3, 1) (-1, 1)
            match (sx, sy) (dx, dy) = go' (2 :: Int) sx sy where
                go' 0 _ _ = Just $ Win piece
                go' n x y
                    | lookup' (x, y) == Just piece = go' (n-1) (x+dx) (y+dy)
                    | otherwise                   = Nothing

            lookup' (x,y) = Map.lookup (Coord x, Coord y) mp'


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
    (p1,p2) <- atomically $ liftM2 (,) (nextConnected queue) (nextConnected queue)
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

    go :: Cyclic t t' => Player t -> Player t' -> Game t -> IO ()
    go p p' (Game b t) = sendBoard >> foldGameState turn draw win t where
        turn f = loop where
            loop        = getMove p >>= resolveMove
            resolveMove = join . atomically . foldFuture disco nextTurn
            disco       = atomically $ notify (userConn p') WonGame
            nextTurn m  = maybe loop (go p' p) (f m)

        draw = atomically $ both $ \u -> notify (userConn u) DrawGame

        win _ = atomically $ do
            notify (userConn p) LostGame
            notify (userConn p') WonGame

        sendBoard = atomically $ both $ \u -> notify (userConn u)  (GameBoard b)

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
