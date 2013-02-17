{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Protocol where

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as JSON
import qualified Data.Map as Map

import Network.WebSockets.Messaging (Message)
import GHC.Generics (Generic)

import Game.Types (Board(..), Coord(..), Piece(..), Move(..))

data ServerRequest
    = AskName
    | AskMove
    | AskNewGame
    | FoundOpponent String
    | GameBoard Board
    | WonGame
    | LostGame
    | DrawGame
    deriving Generic

instance Message ServerRequest

deriving instance ToJSON (Move t)
deriving instance FromJSON (Move t)

instance ToJSON Coord where
    toJSON (Coord i) = toJSON i

instance FromJSON Coord where
    parseJSON js = do
        i <- parseJSON js
        if (i >= 1 && i <= 3)
            then return $ Coord i
            else fail "invalid coordinate"

instance ToJSON Piece where
    toJSON X = JSON.String "X"
    toJSON O = JSON.String "O"

instance FromJSON Piece where
    parseJSON (JSON.String "X") = return X
    parseJSON (JSON.String "O") = return O
    parseJSON _ = fail "invalid Piece"

instance ToJSON Board where
    toJSON (Board mp) = toJSON $
        [ [Map.lookup (Coord r, Coord c) mp| r <- [1..3]]
        | c <- [1..3]
        ]

instance FromJSON Board where
    parseJSON js = do
        rows <- parseJSON js
        let assocs = do
                (r, row) <- zip [1..3] rows
                (c, piece) <- zip [1..3] row
                case piece of
                    Just p -> return ((Coord r, Coord c), p)
                    _      -> []
        return $ Board $ Map.fromList assocs

