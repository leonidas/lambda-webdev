{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status

app :: Application
app req = return $ responseLBS ok200 [] "Hello world!"

main = run 8000 app
