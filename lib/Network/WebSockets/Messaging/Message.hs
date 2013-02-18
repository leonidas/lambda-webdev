{-# LANGUAGE GADTs #-}

module Network.WebSockets.Messaging.Message
    ( Notify(..)
    , Request(..)
    , Some(..)
    ) where

import Data.Aeson

data Some t where
    Some :: (FromJSON x, ToJSON x) => t x -> Some t

class Request r where
    reqToJSON   :: r a -> Value
    reqFromJSON :: Value -> Result (Some r)

class Notify n where
    ntfyToJSON   :: n -> Value
    ntfyFromJSON :: Value -> Result n
