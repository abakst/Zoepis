module ZRandom where

import System.Random.Mersenne.Pure64
import Control.Monad.Random
import Control.Monad
import GHC.Word

type ZRandGen = PureMT
type ZRandom a = Rand ZRandGen a

zRandGen :: Word64 -> ZRandGen
zRandGen = pureMT

zGetRandomR :: Random a => (a, a) -> ZRandom a
zGetRandomR = getRandomR

zRunRand :: ZRandom a -> ZRandGen -> (a, ZRandGen)
zRunRand = runRand

