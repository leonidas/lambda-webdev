{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Game.Protocol where

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as JSON

import Network.WebSockets.Messaging (deriveMessage)

import Game.Board (Board)
import Game.Move (Move)

data ServerRequest resp where
    AskName    :: ServerRequest String
    AskMove    :: ServerRequest (Move t)
    AskNewGame :: ServerRequest Bool

data ServerNotify
    = FoundOpponent String
    | GameBoard Board
    | GameOver GameResult

data GameResult = WonGame | LostGame | DrawGame

deriveMessage ''ServerRequest
deriveMessage ''ServerNotify

instance ToJSON GameResult where
    toJSON WonGame  = "won"
    toJSON LostGame = "lost"
    toJSON DrawGame = "draw"

instance FromJSON GameResult where
    parseJSON (JSON.String "won")  = return WonGame
    parseJSON (JSON.String "lost") = return LostGame
    parseJSON (JSON.String "draw") = return DrawGame
    parseJSON _ = fail "Invalid game result"

