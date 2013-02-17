{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Logic (playGame, assignSides) where

import Control.Applicative ((<|>), (<$>))
import Control.Monad (msum, void, join, when)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Concurrent (forkIO)

import qualified Data.Map as Map

import Network.WebSockets.Messaging (Future, foldFuture, requestAsync, notify, request)

import Unsafe.Coerce

import Game.Types
import Game.Protocol (ServerRequest(..))

type NewPlayer    = User Nothing
type Player piece = User (Just piece)

type Cyclic a b = (a ~ Other b, b ~ Other a)

type CyclicMove turn =
    ( Other (Other turn) ~ turn
    , MoveAssoc turn
    , MoveAssoc (Other turn)
    )

maybeIf :: Bool -> a -> Maybe a
maybeIf p a
    | p         = Just a
    | otherwise = Nothing

getMove :: Player piece -> IO (Future (Move piece))
getMove (User{..}) = requestAsync userConn AskMove

assignSides :: NewPlayer -> NewPlayer -> (Player X, Player O)
assignSides pl1 pl2 = (unsafeCoerce pl1, unsafeCoerce pl2)

stripSide :: Player t -> NewPlayer
stripSide = unsafeCoerce

newGame :: Game X
newGame = Game newBoard $ Turn $ makeMove newBoard where

makeMove :: CyclicMove turn => Board -> Move turn -> Maybe (Game (Other turn))
makeMove (Board mp) move
    | pos `Map.member` mp = Nothing
    | x < 1 || x > 3 || y < 1 || y > 3 = Nothing
    | otherwise = Game board' <$> (gameOver <|> nextTurn)
    where
        assoc@(pos, piece) = moveAssoc move
        (Coord x, Coord y) = pos

        board'   = Board mp'
        mp'      = uncurry Map.insert assoc mp

        nextTurn = Just $ Turn $ makeMove board'
        gameOver = victory <|> draw
        draw     = maybeIf (Map.size mp' == 9) Draw
        victory  = fullRow <|> fullColumn <|> fullDiagonal

        fullRow      = msum [match (1, r) (1, 0) | r <- [1..3]]
        fullColumn   = msum [match (c, 1) (0, 1) | c <- [1..3]]
        fullDiagonal = match (1, 1) (1, 1) <|> match (3, 1) (-1, 1)

        match (sx, sy) (dx, dy) = go (2 :: Int) sx sy where
            go 0 _ _ = Just $ Win piece
            go n x y
                | lookup' (x, y) == Just piece = go (n-1) (x+dx) (y+dy)
                | otherwise                    = Nothing

        lookup' (x,y) = Map.lookup (Coord x, Coord y) mp'

newBoard :: Board
newBoard = Board $ Map.fromList
    [ ((Coord 1, Coord 1), X)
    , ((Coord 2, Coord 1), O)
    ]
-- newBoard = Board $ Map.empty

playGame :: TChan NewPlayer -> (Player X, Player O) -> IO ()
playGame queue (px, po) = start >> play >> both requeue where

    start = atomically $ do
        notify (userConn px) $ FoundOpponent $ userName po
        notify (userConn po) $ FoundOpponent $ userName px

    play = go px po newGame

    go :: Cyclic t t' => Player t -> Player t' -> Game t -> IO ()
    go p p' (Game b st) = sendBoard >> foldGameStatus turn draw win st where
        turn f = loop where
            loop        = getMove p >>= resolveMove
            resolveMove = join . atomically . foldFuture disconnect nextTurn
            disconnect  = atomically $ notify (userConn p') WonGame
            nextTurn m  = maybe loop (go p' p) (f m)

        draw = atomically $ both $ \u -> notify (userConn u) DrawGame

        win _ = atomically $ do
            notify (userConn p) LostGame
            notify (userConn p') WonGame

        sendBoard = atomically $ both $ \u -> notify (userConn u) (GameBoard b)

    both :: Monad m => (forall t.Player t -> m ()) -> m ()
    both op = op px >> op po

    requeue :: Player t -> IO ()
    requeue p = void $ forkIO $ do
        yes <- request (userConn p) AskNewGame
        when yes $ atomically $ writeTChan queue $ stripSide p
