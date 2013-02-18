{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Move (requestMove, Move, movePos) where

import Data.Aeson (ToJSON, FromJSON)
import Network.WebSockets.Messaging (Future, requestAsync)

import Game.Piece (Piece(..))
import Game.User (User(..))
import Game.Board (Position)
import Game.Protocol (ServerRequest(AskMove))

newtype Move (piece :: Piece) = Move { movePos :: Position }
    deriving (ToJSON, FromJSON)

requestMove :: User (Just piece) -> IO (Future (Move piece))
requestMove u = requestAsync (userConn u) AskMove

