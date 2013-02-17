{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Types where

import Data.Map (Map)

data Board = Board (Map Position Piece)
data Coord = Coord Int deriving (Eq, Ord)
type Position = (Coord, Coord)

data Piece = X | O deriving (Eq)

type family Other (p :: Piece) :: Piece
type instance Other X = O
type instance Other O = X

