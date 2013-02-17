{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.WebSockets.Messaging.Container where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)

type ReqId = Int

data Container
    = ProtocolError !Text
    | ProtocolDebug !Text
    | Request !ReqId !Value
    | Response !ReqId !Value
    | Notification !Value

instance ToJSON Container where
    toJSON c = toJSON $ case c of
        ProtocolError msg     -> [String "error",    toJSON msg]
        ProtocolDebug msg     -> [String "debug",    toJSON msg]
        Request rqId payload  -> [String "request",  toJSON rqId, payload]
        Response rqId payload -> [String "response", toJSON rqId, payload]
        Notification payload  -> [String "notify",   payload]

instance FromJSON Container where
    parseJSON js = parseJSON js >>= \lst -> case lst of
        [String "error", msg]              -> ProtocolError <$> parseJSON msg
        [String "debug", msg]              -> ProtocolDebug <$> parseJSON msg
        [String "request", rqId, payload]  -> Request       <$> parseJSON rqId <*> pure payload
        [String "response", rqId, payload] -> Response      <$> parseJSON rqId <*> pure payload
        [String "notify", payload]         -> return $ Notification payload
        _                                  -> fail "invalid container"

