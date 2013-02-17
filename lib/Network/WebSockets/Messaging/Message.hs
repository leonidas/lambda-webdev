{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverlappingInstances #-}

module Network.WebSockets.Messaging.Message (Message(..)) where

import Control.Monad (guard)
import Control.Applicative ((<|>))
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.String

class Message m where
    msgToJSON   :: m -> Value
    msgFromJSON :: Value -> Result m

    default msgToJSON :: (Generic m, GToJSON (Rep m)) => m -> Value
    msgToJSON = gToJSON . from

    default msgFromJSON :: (Generic m, GFromJSON (Rep m)) => Value -> Result m
    msgFromJSON = fmap to . parse (gParseJSON undefined)


class GToJSON g where
    gToJSON :: g x -> Value


class GFromJSON g where
    gParseJSON :: g x -> Value -> Parser (g x)


instance (GToJSON a) => GToJSON (M1 D d a) where
    gToJSON (M1 a) = gToJSON a

instance (Constructor c, GToJSON a) => GToJSON (M1 C c a) where
    gToJSON c@(M1 a) = object ["msg" .= typ, "data" .= gToJSON a] where
        typ = conName c

instance (GToJSON a, GToJSON b) => GToJSON (a :+: b) where
    gToJSON (L1 a) = gToJSON a
    gToJSON (R1 b) = gToJSON b

instance (GListFields a, GListFields b) => GToJSON (a :*: b) where
    gToJSON (a :*: b) = object $ gListFields a ++ gListFields b

instance (ToJSON a) => GToJSON (M1 S NoSelector (K1 k a)) where
    gToJSON (M1 (K1 a)) = toJSON a

instance GToJSON U1 where
    gToJSON _ = object []

class GListFields g where
    gListFields :: g x -> [Pair]

instance (GToJSON a, Selector s) => GToJSON (M1 S s a) where
    gToJSON = object . gListFields

instance (GToJSON a, Selector s) => GListFields (M1 S s a) where
    gListFields s@(M1 a) = [fromString (selName s) .= gToJSON a]

instance ToJSON a => GToJSON (K1 k a) where
    gToJSON (K1 a) = toJSON a

instance GFromJSON a => GFromJSON (M1 D d a) where
    gParseJSON _ = fmap M1 . gParseJSON undefined

instance (Constructor c, GFromJSON a) => GFromJSON (M1 C c a) where
    gParseJSON c (Object o) = do
        String typ <- o .: "msg"
        guard $ typ == (fromString $ conName c)
        dat <- o .: "data"
        fmap M1 (gParseJSON undefined dat)

    gParseJSON _ _ = fail "Message must be a JS object"

instance GFromJSON U1 where
    gParseJSON _ js | js == emptyObject = return U1
    gParseJSON _ _ = fail "non-empty data for unit message"

instance (GFromJSON a, GFromJSON b) => GFromJSON (a :*: b) where
    gParseJSON _ js = do
        a <- gParseJSON undefined js
        b <- gParseJSON undefined js
        return $ a :*: b

instance FromJSON a => GFromJSON (M1 S NoSelector (K1 k a)) where
    gParseJSON _ = fmap (M1 . K1) . parseJSON

instance (GFromJSON a, Selector s) => GFromJSON (M1 S s a) where
    gParseJSON s (Object o) = do
        let n = selName s
        v <- o .: fromString n
        fmap M1 $ gParseJSON undefined v

    gParseJSON _ _ = fail "Message data must be an object"

instance FromJSON a => GFromJSON (K1 k a) where
    gParseJSON _ = fmap K1 . parseJSON

instance (GFromJSON a, GFromJSON b) => GFromJSON (a :+: b) where
    gParseJSON _ js = l1 <|> r1 where
        l1 = fmap L1 (gParseJSON undefined js)
        r1 = fmap R1 (gParseJSON undefined js)

{-
data Test = Test String deriving (Generic, Show)

instance Message Test

test = msgToJSON $ Test "foobar"

data TestMessage = TestMessage deriving (Generic, Show)
data TestMessage2 = TestMessage2 { foo :: Int, bar :: String } deriving (Generic, Show)
data Test3 = Test3A { foo3 :: Int } | Test3B { bar3 :: String } deriving (Generic, Show)

instance Message TestMessage where
instance Message TestMessage2 where
instance Message Test3 where

test = msgFromJSON $ msgToJSON TestMessage :: Result TestMessage
test2 = msgFromJSON $ msgToJSON $ TestMessage2 5 "foo" :: Result TestMessage2
test3a = msgFromJSON $ msgToJSON $ Test3A 10 :: Result Test3
test3b = msgFromJSON $ msgToJSON $ Test3B "bar" :: Result Test3
-}
