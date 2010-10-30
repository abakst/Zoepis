module ZParticle where

import ZObject
import ZVector
import ZGraphics
import ZRandom

import Control.Applicative
import Control.Monad
import Graphics.Rendering.OpenGL    
import Data.Map
import GHC.Word

type ZRandomParticle = ZRandom ZParticle

data ZParticle = ZDeadParticle
               | ZParticle {
                   pLife  :: GLfloat
                 , pColor :: (GLfloat, GLfloat, GLfloat)
                 , pPos   :: Point3D GLdouble
                 , pVel   :: Vector3D GLdouble
                 , pSize  :: GLdouble
                 }

data ZParticleEngine = ZParticleEngine {
      pEngF    :: ZParticle -> ZRandomParticle
    , pEngPs   :: ![ZParticle]
    , pEngRand :: !ZRandGen
    }
                       
zNewParticleEngine :: Int ->
                      Int ->
                      Word64 ->
                      ZRandomParticle ->
                     (ZParticle -> ZRandomParticle) ->
                     ZParticleEngine
zNewParticleEngine dl num seed newpart update = 
    ZParticleEngine {
  pEngF = update
, pEngPs = ps
, pEngRand = g'
}
    where (ps, g') = zRunRand (sequence (replicate num newpart))
                     (zRandGen seed)

zNewParticle :: Double -> Double -> Double ->
                ZRandom (GLfloat, GLfloat, GLfloat) ->
                ZRandomParticle
zNewParticle baseSpread theta str col =
    do life <- toGL $ zGetRandomR (0.85, 0.100::Double)
       x <- toGL $ zGetRandomR (-baseSpread, baseSpread)
       y <- toGL $ zGetRandomR (-baseSpread, baseSpread)
       z <- toGL $ zGetRandomR (-baseSpread, baseSpread)
       vy <-  toGL $ zGetRandomR (0.0, str)
       vx <- toGL $ zGetRandomR (-cone, cone)
       vz <- toGL $ zGetRandomR (-cone, cone)
       color <- col
       return $ ZParticle {
                    pLife = life
                  , pColor = color
                  , pPos  = vector3D (x,y,z)
                  , pVel  = vector3D (vx,vy,vz)
                  , pSize = 1
                  }
    where
      toGL :: (Fractional b, Monad m) => m Double -> m b
      toGL = liftM realToFrac
      cone = str * sin theta


zStepParticle :: ZRandomParticle -> Double -> ZParticle ->
                 ZRandomParticle
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
  

-- zParticleEngine :: [ZParticle] ->
--                   Int ->
--                   ZPartRandGen ->
--                  (ZParticle -> ZRandomParticle) ->
--                  (ZPartRandGen, ZParticleEngine)
-- zParticleEngine ps dl randgen update =
--     (randgen, ZParticleEngine {
--                 , pEngF = update
--                 , pEngPs = ps
--                 , pEngRand = randgen
--                 })

renderParticles :: ZGraphicsResources -> ZParticleEngine -> IO ()
renderParticles res peng = do
    blend $= Enabled
    blendFunc $= (SrcAlpha, One)
    lighting $= Disabled
    pointSprite $= Enabled
    pointSize $= 5
    renderPrimitive Points $ do
      mapM_ (drawParticle res) (pEngPs peng)
    lighting $= Enabled
    blend $= Disabled
    
drawParticle _ ZDeadParticle = return ()
drawParticle _ p = do
  let (r,g,b) = pColor p
  color $ Color4 r g b (pLife p)
  vertex (fromVec3D $ pPos p :: Vertex3 GLdouble)
    
updateParticles :: ZParticleEngine -> ZParticleEngine
updateParticles peng =
    let (ps', g') = zRunRand (mapM (pEngF peng) (pEngPs peng)) (pEngRand peng)
    in peng { pEngPs = ps', pEngRand = g' }
                    



       
                           
  