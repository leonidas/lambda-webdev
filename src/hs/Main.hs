{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp

import Network.Wai.Application.Static
import WaiAppStatic.Types

app :: Application
app = staticApp $ (defaultFileServerSettings "public") { ssMaxAge = MaxAgeSeconds 1 }

main :: IO ()
main = run 8000 app
