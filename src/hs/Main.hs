
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsIntercept, settingsPort)
import Network.Wai.Handler.WebSockets (intercept)

import Game.Server (initWSApp, app)

main :: IO ()
main = do
    wsApp <- initWSApp

    let settings = defaultSettings
            { settingsIntercept = intercept wsApp
            , settingsPort      = 8000
            }

    runSettings settings app
