{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsIntercept)

import Network.Wai.Application.Static
import WaiAppStatic.Types

import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS

app :: Application
app = staticApp $ (defaultFileServerSettings "public") { ssMaxAge = MaxAgeSeconds 1 }

wsApp :: WS.Request -> WS.WebSockets WS.Hybi00 ()
wsApp = undefined

main :: IO ()
main = runSettings settings app where
    settings = defaultSettings
        { settingsIntercept = intercept wsApp }
