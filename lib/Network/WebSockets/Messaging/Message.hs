{-# LANGUAGE GADTs #-}

module Network.WebSockets.Messaging.Message (Message(..), Some(..), Request(..)) where

import Data.Aeson

data Some t where
    Some :: (FromJSON x, ToJSON x) => t x -> Some t

class Request r where
    reqToJSON   :: r a -> Value
    reqFromJSON :: Value -> Result (Some r)

class Message m where
    msgToJSON   :: m -> Value
    msgFromJSON :: Value -> Result m
