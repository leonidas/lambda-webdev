{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Piece where

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as JSON


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

instance ToJSON Piece where
    toJSON X = JSON.String "X"
    toJSON O = JSON.String "O"

instance FromJSON Piece where
    parseJSON (JSON.String "X") = return X
    parseJSON (JSON.String "O") = return O
    parseJSON _ = fail "invalid Piece"


