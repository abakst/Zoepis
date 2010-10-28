module ZParticle where

import ZObject
import ZVector
import ZGraphics

import Control.Applicative
import Graphics.Rendering.OpenGL    
import System.Random.Mersenne.Pure64
import Control.Monad.Random
import Control.Monad
import Data.Map
import GHC.Word

type ZPartRandGen = PureMT    
type ZParticleRandom = Rand PureMT ZParticle
zParticleRand = pureMT

data ZParticle = ZDeadParticle
               | ZParticle {
                   pLife :: GLfloat
                 , pPos  :: Point3D GLdouble
                 , pVel  :: Vector3D GLdouble
                 , pSize :: GLdouble
                 }

data ZParticleEngine = ZParticleEngine {
      pEngDL   :: Int
    , pEngF    :: ZParticle -> ZParticleRandom
    , pEngPs   :: ![ZParticle]
    , pEngRand :: !ZPartRandGen
    }
                       
zNewParticleEngine :: Int ->
                     Int ->
                     Word64 ->
                     ZParticleRandom ->
                    (ZParticle -> ZParticleRandom)
                     -> ZParticleEngine
zNewParticleEngine dl num seed newpart update = ZParticleEngine {
                                    pEngDL = dl
                                  , pEngF = update
                                  , pEngPs = ps
                                  , pEngRand = g'
                                  }
    where (ps, g') = runRand (sequence (replicate num newpart))
                             (zParticleRand seed)

zNewParticle :: Double -> Double -> Double -> ZParticleRandom
zNewParticle baseSpread theta str =
    do life <- toGL $ getRandomR (0.85, 0.100::Double)
       x <- toGL $ getRandomR (-baseSpread, baseSpread)
       y <- toGL $ getRandomR (-baseSpread, baseSpread)
       z <- toGL $ getRandomR (-baseSpread, baseSpread)
       vy <-  toGL $ getRandomR (0.0, str)
       vx <- toGL $ getRandomR (-cone, cone)
       vz <- toGL $ getRandomR (-cone, cone)
       return $ ZParticle {
                    pLife = life
                  , pPos  = vector3D (x,y,z)
                  , pVel  = vector3D (vx,vy,vz)
                  , pSize = 1
                  }
    where
      toGL :: (Fractional b, Monad m) => m Double -> m b
      toGL = liftM realToFrac
      cone = str * sin theta


zStepParticle :: ZParticleRandom -> Double -> ZParticle ->
                 ZParticleRandom
zStepParticle newp _ ZDeadParticle   = newp                      
zStepParticle _ gravity p = let life' = pLife p - 0.02
                                pos'  = pPos p + pVel p
                                vel'  = pVel p - ZVector.scale (realToFrac gravity) yAxis
                            in if life' > 0
                               then return $ p { pLife = life'
                                               , pPos = pos'
                                               , pVel = vel'
                                               }
                               else return ZDeadParticle
  

particleEngine :: [ZParticle] ->
                  Int ->
                  ZPartRandGen ->
                 (ZParticle -> ZParticleRandom) ->
                 (ZPartRandGen, ZParticleEngine)
particleEngine ps dl randgen update =
    (randgen, ZParticleEngine {
                  pEngDL = dl
                , pEngF = update
                , pEngPs = ps
                , pEngRand = randgen
                })

renderParticles :: ZGraphicsResources -> ZParticleEngine -> IO ()
renderParticles res peng = do
    blend $= Enabled
    blendFunc $= (SrcAlpha, One)
    lighting $= Disabled
    pointSprite $= Enabled
    pointSize $= 5
    renderPrimitive Points $ do
      mapM_ (drawParticle (pEngDL peng) res) (pEngPs peng)
    lighting $= Enabled
    blend $= Disabled
    
drawParticle _ _ ZDeadParticle = return ()
drawParticle l res p = do
  color $ Color4 0.2 0.3 1 (pLife p)
  vertex (fromVec3D $ pPos p :: Vertex3 GLdouble)
    
updateParticles :: ZParticleEngine -> ZParticleEngine
updateParticles peng =
    let (ps', g') = runRand (mapM (pEngF peng) (pEngPs peng)) (pEngRand peng)
    in peng { pEngPs = ps', pEngRand = g' }
                    



       
                           
  