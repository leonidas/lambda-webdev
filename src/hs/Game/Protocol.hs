{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Game.Protocol where

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as JSON

import Network.WebSockets.Messaging (Message)
import GHC.Generics (Generic)

import Game.Board (Board)

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

