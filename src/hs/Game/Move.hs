{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Move (requestMove, Move, MoveAssoc(..)) where

import Data.Aeson (ToJSON, FromJSON)
import Network.WebSockets.Messaging (Future, requestAsync)

import Game.Types
import Game.Protocol (ServerRequest(AskMove))

newtype Move (piece :: Piece) = Move Position
    deriving (ToJSON, FromJSON)

class MoveAssoc (p :: Piece) where
    moveAssoc :: Move p -> (Position, Piece)
    moveAssoc m@(Move pos) = (pos, movePiece m)

    movePiece :: Move p -> Piece

instance MoveAssoc X where
    movePiece _ = X

instance MoveAssoc O where
    movePiece _ = O

requestMove :: User (Just piece) -> IO (Future (Move piece))
requestMove (User{..}) = requestAsync userConn AskMove

