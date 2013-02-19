
module Game.Random (Random, runRandom, getRandom) where

import Control.Monad.Random hiding (Random)

type Random = Rand StdGen

runRandom :: Random a -> IO a
runRandom = evalRandIO
