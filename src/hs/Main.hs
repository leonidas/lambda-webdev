{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsIntercept, settingsPort)

import Network.Wai.Application.Static (staticApp, defaultFileServerSettings, ssMaxAge)
import WaiAppStatic.Types (MaxAge(..))

import Network.Wai.Handler.WebSockets (intercept)
import qualified Network.WebSockets as WS

import Network.WebSockets.Messaging

-- import Data.Text (Text)

import GHC.Generics (Generic)

import Control.Monad (void)

import Control.Concurrent (forkIO)

import Control.Concurrent.STM

data NameNotification = Name String deriving Generic

instance Message NameNotification

app :: Application
app = staticApp $ (defaultFileServerSettings "public") { ssMaxAge = MaxAgeSeconds 1 }

initWSApp :: IO (WS.Request -> WS.WebSockets WS.Hybi00 ())
initWSApp = do

    queue <- newTChanIO

    void $ forkIO $ matchMaker queue

    return $ \req -> do
        WS.acceptRequest req
        onConnect $ \conn -> do
            let handleName (Name n) = putStrLn n

            atomically $ do
                onNotify conn handleName
                writeTChan queue conn

            return ()

matchMaker :: TChan Connection -> IO ()
matchMaker = undefined

main :: IO ()
main = do
    wsApp <- initWSApp

    let settings = defaultSettings
            { settingsIntercept = intercept wsApp
            , settingsPort      = 8000
            }

    runSettings settings app
