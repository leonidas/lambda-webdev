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

import Game.Types (Board(..), Coord(..), Piece(..))

data ServerRequest
    = AskName
    | AskMove
    | AskNewGame
    | FoundOpponent String
    | GameBoard Board
    | GameOver GameResult
    deriving Generic

data GameResult = WonGame | LostGame | DrawGame

instance Message ServerRequest

instance ToJSON GameResult where
    toJSON WonGame  = "won"
    toJSON LostGame = "lost"
    toJSON DrawGame = "draw"

instance FromJSON GameResult where
    parseJSON (JSON.String "won")  = return WonGame
    parseJSON (JSON.String "lost") = return LostGame
    parseJSON (JSON.String "draw") = return DrawGame
    parseJSON _ = fail "Invalid game result"

instance ToJSON Coord where
    toJSON (Coord i) = toJSON i

instance FromJSON Coord where
    parseJSON js = do
        i <- parseJSON js
        if (i >= 1 && i <= 3)
            then return $ Coord i
            else fail "invalid coordinate"

instance ToJSON Piece where
    toJSON X = JSON.String "X"
    toJSON O = JSON.String "O"

instance FromJSON Piece where
    parseJSON (JSON.String "X") = return X
    parseJSON (JSON.String "O") = return O
    parseJSON _ = fail "invalid Piece"

instance ToJSON Board where
    toJSON (Board mp) = toJSON $ Map.assocs mp

instance FromJSON Board where
    parseJSON = fmap (Board . Map.fromList) . parseJSON

