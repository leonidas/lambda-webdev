
module Game.Board where

import Data.Map (Map)

import Game.Piece (Piece(..))

data Board = Board (Map Position Piece)
data Coord = Coord Int deriving (Eq, Ord)
type Position = (Coord, Coord)

