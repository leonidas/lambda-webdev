{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Game.User
    ( newUser
    , userName
    , userConn
    , assignSides
    , stripSide
    , User
    , Player
    , NewPlayer
    ) where

import Unsafe.Coerce
import Network.WebSockets.Messaging (Connection)

import Game.Piece (Piece(..))
import Game.Random

data User (piece :: Maybe Piece) = User
    { userName :: String
    , userConn :: Connection
    }

type NewPlayer    = User Nothing
type Player piece = User (Just piece)

newUser :: String -> Connection -> NewPlayer
newUser = User

assignSides :: NewPlayer -> NewPlayer -> Random (Player X, Player O)
assignSides pl1 pl2 = do
    swap <- getRandom
    return $ if swap
        then (unsafeCoerce pl2, unsafeCoerce pl1)
        else (unsafeCoerce pl1, unsafeCoerce pl2)

stripSide :: Player t -> NewPlayer
stripSide = unsafeCoerce


