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
    ) where

import Unsafe.Coerce
import Network.WebSockets.Messaging (Connection)

import Game.Types

data User (piece :: Maybe Piece) = User
    { userName :: String
    , userConn :: Connection
    }

newUser :: String -> Connection -> User Nothing
newUser = User

assignSides :: User Nothing -> User Nothing -> (User (Just X), User (Just O))
assignSides pl1 pl2 = (unsafeCoerce pl1, unsafeCoerce pl2)

stripSide :: User (Just t) -> User Nothing
stripSide = unsafeCoerce


