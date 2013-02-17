{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
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

class ReifyPiece (p :: Piece) where
    reifyPiece :: f p -> Piece

instance ReifyPiece X where
    reifyPiece _ = X

instance ReifyPiece O where
    reifyPiece _ = O

type CyclicPiece piece =
    ( Other (Other piece) ~ piece
    , ReifyPiece piece
    , ReifyPiece (Other piece)
    )


