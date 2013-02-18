{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Move (Move, movePos) where

import Data.Aeson (ToJSON, FromJSON)

import Game.Piece (Piece(..))
import Game.Board (Position)

newtype Move (piece :: Piece) = Move { movePos :: Position }
    deriving (ToJSON, FromJSON)

