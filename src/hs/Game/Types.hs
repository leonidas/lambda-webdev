{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Game.Types where

import Data.Map (Map)

import Network.WebSockets.Messaging (Connection)


data Board = Board (Map Position Piece)

data Coord = Coord Int deriving (Eq, Ord)

type Position = (Coord, Coord)
data Piece = X | O deriving (Eq)

type family Other (p :: Piece) :: Piece
type instance Other X = O
type instance Other O = X

newtype Move (piece :: Piece) = Move Position

data User (piece :: Maybe Piece) = User
    { userName :: String
    , userConn :: Connection
    }

data Game turn = Game Board (GameStatus turn)

data GameStatus (turn :: Piece) where
    Turn  :: ProcessMove turn -> GameStatus turn
    Draw  :: GameStatus a
    Win   :: Piece -> GameStatus a

foldGameStatus
    :: (ProcessMove turn -> r)
    -> r
    -> (Piece -> r)
    -> GameStatus turn
    -> r
foldGameStatus handleTurn handleDraw handleWin s = case s of
    Turn f -> handleTurn f
    Draw   -> handleDraw
    Win p  -> handleWin p

type ProcessMove turn = Move turn -> Maybe (Game (Other turn))

class MoveAssoc (p :: Piece) where
    moveAssoc :: Move p -> (Position, Piece)
    moveAssoc m@(Move pos) = (pos, movePiece m)

    movePiece :: Move p -> Piece

instance MoveAssoc X where
    movePiece _ = X

instance MoveAssoc O where
    movePiece _ = O



