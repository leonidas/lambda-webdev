{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Game.Types where

import Data.Map (Map)
import Unsafe.Coerce

import Network.WebSockets.Messaging (Connection)


data Board = Board (Map Position Piece)
data Coord = Coord Int deriving (Eq, Ord)
type Position = (Coord, Coord)

data Piece = X | O deriving (Eq)

type family Other (p :: Piece) :: Piece
type instance Other X = O
type instance Other O = X

data User (piece :: Maybe Piece) = User
    { userName :: String
    , userConn :: Connection
    }

assignSides :: User Nothing -> User Nothing -> (User (Just X), User (Just O))
assignSides pl1 pl2 = (unsafeCoerce pl1, unsafeCoerce pl2)

stripSide :: User (Just t) -> User Nothing
stripSide = unsafeCoerce


