{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}

module Network.WebSockets.Messaging.Connection where

import Network.WebSockets hiding (send, Request, Message)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Control.Applicative
import Control.Monad (guard, forever, void, (>=>), mplus)
import Control.Monad.IO.Class

import Data.Aeson (encode, decode, ToJSON(..), FromJSON(..), fromJSON, Result(..))
import qualified Data.Aeson as Json
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Control.Exception (catch)
import Prelude hiding (catch)

import Network.WebSockets.Messaging.Container
import Network.WebSockets.Messaging.Message

type Closable c a = c (Maybe a)

type Handler r = Json.Value -> STM (IO r)

type SubId = Int

data Connection = Connection
    { outbox       :: !(Closable TQueue Json.Value)
    , disconnected :: !(TVar Bool)
    , subId        :: !(TVar SubId)
    , requestSubs  :: !(TVar (IntMap (Handler Json.Value)))
    , notifySubs   :: !(TVar (IntMap (Handler ())))
    , reqId        :: !(TVar ReqId)
    , reqMap       :: !(TVar (IntMap (TMVar Json.Value)))
    }

data Future a = Future !(TMVar a) !(STM Bool)

get :: Future a -> STM a
get (Future var _) = readTMVar var

foldFuture :: r -> (a -> r) -> Future a -> STM r
foldFuture discHandler resHandler (Future var disc) =
    fmap resHandler (readTMVar var) `orElse` do
        d <- disc
        if d
            then return discHandler
            else retry



newConnection :: STM Connection
newConnection = Connection
    <$> newTQueue
    <*> newTVar False
    <*> newTVar 0
    <*> newTVar IntMap.empty
    <*> newTVar IntMap.empty
    <*> newTVar 0
    <*> newTVar IntMap.empty

requestAsync :: (Request req, FromJSON resp) => Connection -> req resp -> IO (Future resp)
requestAsync conn@(Connection {..}) !req = do
    resp <- newEmptyTMVarIO
    fut  <- newEmptyTMVarIO

    void $ forkIO $ do

        rqId <- atomically $ do
            rqId <- nextReqId conn
            modifyTVar' reqMap $! IntMap.insert rqId resp
            send conn $! Request rqId $! reqToJSON req
            return rqId

        js <- atomically $ do
            modifyTVar' reqMap $! IntMap.delete rqId
            readTMVar resp

        case fromJSON js of
            Json.Success dat -> atomically $! putTMVar fut $! dat
            Json.Error msg   -> do
                atomically $! send conn $! ProtocolError $! T.pack msg
                error "malformed response"

    return $ Future fut (readTVar disconnected)


request :: (Request req, FromJSON resp) => Connection -> req resp -> IO resp
request conn@(Connection {..}) !req = do
    rqId <- atomically $ do
        rqId' <- readTVar reqId
        writeTVar reqId $! rqId' + 1
        return rqId'

    resp <- newEmptyTMVarIO
    atomically $ do
        modifyTVar' reqMap $! IntMap.insert rqId resp
        send conn $! Request rqId $! reqToJSON req

    js <- atomically $ do
        modifyTVar' reqMap $! IntMap.delete rqId
        readTMVar resp

    case fromJSON js of
        Json.Success dat -> return dat
        Json.Error msg   -> do
            atomically $! send conn $! ProtocolError $! T.pack msg
            error "malformed response"

notify :: Notify ntfy => Connection -> ntfy -> STM ()
notify conn = send conn . Notification . ntfyToJSON

nextSubId :: Connection -> STM SubId
nextSubId (Connection {..}) = do
    sId  <- readTVar subId
    writeTVar subId $! sId + 1
    return sId

nextReqId :: Connection -> STM SubId
nextReqId (Connection {..}) = do
    rqId  <- readTVar reqId
    writeTVar reqId $! rqId + 1
    return rqId


onRequest :: Request req
    => Connection
    -> (forall resp. req resp -> IO resp)
    -> STM ()
onRequest conn@(Connection {..}) !handler = do
    sid <- nextSubId conn
    modifyTVar' requestSubs (IntMap.insert sid handler') where
        handler' js = case reqFromJSON js of
            Json.Success (Some rq) -> return $! toJSON <$> handler rq
            Error _                -> retry

onNotify :: Notify ntfy => Connection -> (ntfy -> IO ()) -> STM ()
onNotify conn@(Connection{..}) !handler = do
    sid <- nextSubId conn
    modifyTVar' notifySubs (IntMap.insert sid handler') where
        handler' js = case ntfyFromJSON js of
            Json.Success ntfy -> return $! handler ntfy
            Error _           -> retry

onDisconnect :: Connection -> STM () -> STM ()
onDisconnect !(Connection {..}) !handler =
    readTVar disconnected >>= guard >> handler

send :: Connection -> Container -> STM ()
send (Connection {..}) = writeTQueue outbox . Just . toJSON

recvJson :: (TextProtocol p, FromJSON a) => WebSockets p (Maybe a)
recvJson = decode <$> receiveData

sendJson :: TextProtocol p => Json.Value -> WebSockets p ()
sendJson = sendTextData . encode

sinkJson :: TextProtocol p => Sink p -> Json.Value -> IO ()
sinkJson sink = sendSink sink . DataMessage . Text . encode

untilClosed :: Closable TQueue a -> (a -> STM b) -> (b -> IO c) -> IO ()
untilClosed chan handler after = loop where
    loop =
        atomically (readTQueue chan >>= traverse handler)
        >>= traverse_ (after >=> const loop)

dispatch ::  Connection -> Container -> IO ()
dispatch conn@(Connection {..}) !c = case c of
    Request rqId js  -> do
        handler <- atomically $ do
            subs <- readTVar requestSubs
            let trySubs = foldr mplus retry $ map ($ js) $ IntMap.elems subs
            fmap Just trySubs `orElse` return Nothing

        void $ forkIO $ maybe invalidRequest respond handler

        where
            invalidRequest = atomically . send conn
                $ ProtocolError "unrecognized request"

            respond h = h >>= atomically . send conn . Response rqId

    Notification js -> do
        handler <- atomically $ do
            subs <- readTVar notifySubs
            let trySubs = foldr mplus retry $ map ($ js) $ IntMap.elems subs
            fmap Just trySubs `orElse` return Nothing

        void $ forkIO $ fromMaybe noHandler handler

        where
            noHandler = atomically . send conn
                $ ProtocolDebug "ignored notification"

    Response rqId js -> atomically $ do
        h <- IntMap.lookup rqId <$> readTVar reqMap
        case h of
            Nothing  -> responseIgnored
            Just var -> putTMVar var js
        where
            responseIgnored = send conn $ ProtocolDebug "ignored response"

    _ -> return () -- TODO: print/log error?

onConnect :: TextProtocol p => (Connection -> IO ()) -> WebSockets p ()
onConnect handler = do
    conn@(Connection {..}) <- liftIO $ atomically newConnection
    let replyInvalid = atomically $ send conn $ ProtocolError "invalid message"

        handleWriteError (_ :: ConnectionError) = signalDisconnect
        handleReadError _ = liftIO signalDisconnect
        signalDisconnect = do
            atomically $ do
                writeTQueue outbox Nothing
                writeTVar disconnected True

        readLoop = forever $ do
            recvJson >>= liftIO . maybe replyInvalid (dispatch conn)

    sink <- getSink

    liftIO $ do
        void . forkIO $ untilClosed outbox return (sinkJson sink)
            `catch` handleWriteError

        void . forkIO $ handler conn

    catchWsError readLoop handleReadError
