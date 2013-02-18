{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Game.Board
    ( newBoard
    , positions
    , lanes
    , putPiece
    , isFull
    , (!)
    , Board
    , Position
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (transpose)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Game.Piece (Piece(..))


newtype Board = Board (Map Position Piece)
newtype Position = Position (Int, Int)
    deriving (Eq, Ord, Show)

newBoard :: Board
newBoard = Board $ Map.empty

putPiece :: Piece -> Position -> Board -> Board
putPiece pc pos (Board mp) = Board $ Map.insert pos pc mp

isFull :: Board -> Bool
isFull (Board mp) = Map.size mp == 9

(!) :: Board -> Position -> Maybe Piece
Board mp ! pos = Map.lookup pos mp

positions :: [Position]
positions = concat rows

lanes :: [[Position]]
lanes = rows ++ cols ++ diagonals

rows :: [[Position]]
rows = [[Position (c, r) | c <- [1..3]] | r <- [1..3]]

cols :: [[Position]]
cols = transpose rows

diagonals :: [[Position]]
diagonals = [zipWith (!!) rows [0..], zipWith (!!) (reverse rows) [0..]]

instance FromJSON Position where
    parseJSON js = do
        pos@(c, r) <- parseJSON js
        if c < 1 || r < 1 || c > 3 || r > 3
            then fail $ "invalid position " ++ show pos
            else return $ Position pos

instance ToJSON Position where
    toJSON (Position pos) = toJSON pos

instance ToJSON Board where
    toJSON (Board mp) = toJSON $ Map.assocs mp

instance FromJSON Board where
    parseJSON = fmap (Board . Map.fromList) . parseJSON

