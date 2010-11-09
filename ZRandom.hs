module ZRandom where

import System.Random.Mersenne.Pure64
import System.IO.Unsafe
import Control.Monad.Random
import Control.Monad
import GHC.Word
import Data.Array

type ZRandGen = PureMT
type ZRandom a = Rand ZRandGen a
type ZRandomC a = Rand ZRandGenC a

zRandGen :: Word64 -> ZRandGen
zRandGen = pureMT

zGetRandomR :: (Random a, MonadRandom m) => (a, a) -> m a
zGetRandomR = getRandomR

--zRunRand :: ZRandom a -> ZRandGen -> (a, ZRandGen)
zRunRand :: (RandomGen g) => Rand g a -> g -> (a, g)
zRunRand = runRand

data ZRandGenC = ZRandGenC {
      zrcI :: !Int
    , zrcRs :: !(Array Int Int)
    , zrcRange :: !(Int, Int)
    , zrcNum :: !Int
    , zrcRNG :: StdGen
    }
                   
zRandGenC :: Int -> Int -> ZRandGenC
zRandGenC seed num = ZRandGenC {
                       zrcI = 1
                     , zrcRs = rvals
                     , zrcRange = theRange
                     , zrcNum = num
                     , zrcRNG = rng0        
                     } 
    where rvals = listArray (1,num) vals
          (vals,rng0) = runRand (getRandomRs theRange) (mkStdGen seed)
          theRange = genRange (mkStdGen seed)
          
fromGen gen num = ZRandGenC {
                    zrcI = 1
                  , zrcRs = rvals
                  , zrcRange = genRange gen
                  , zrcNum = num
                  , zrcRNG = gen'
                  }
    where (vals, gen') = runRand (getRandomRs (genRange gen)) gen
          rvals = listArray (1,num) vals
          
instance RandomGen ZRandGenC where
    next g = (zrcRs g ! zrcI g, g { zrcI = (zrcI g + 1) `mod` (zrcNum g) + 1})
    split g = let (g1, g2) = split (zrcRNG g)
              in (fromGen g1 (zrcNum g), fromGen g2 (zrcNum g))
