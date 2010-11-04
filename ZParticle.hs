module ZParticle where

import ZObject
import ZVector
import ZGraphics
import ZRandom

import Control.Applicative
import Control.Monad
import Graphics.Rendering.OpenGL as GL
import Data.Map
import GHC.Word
import Graphics.Rendering.OpenGL.Raw
import Foreign.Marshal.Array

type ZRandomParticle = ZRandom ZParticle

data ZParticle = ZDeadParticle
               | ZParticle {
                   pLife  :: GLfloat
                 , pColor :: (GLfloat, GLfloat, GLfloat)
                 , pPos   :: Point3D GLfloat
                 , pVel   :: Vector3D GLfloat
                 , pSize  :: GLfloat
                 }

data ZParticleEngine = ZParticleEngine {
      pEngF    :: ZParticle -> ZRandomParticle
    , pEngNewP :: Point3D Float -> Vector3D Float -> ZRandomParticle
    , pEngPs   :: ![ZParticle]
    , pEngRand :: !ZRandGen
    , pEngPos  :: Point3D Float
    , pEngDir  :: Vector3D Float
    }

zNewParticleEngine :: Int ->
                      Word64 ->
                      Point3D Float ->
                      Vector3D Float ->
                      Float ->
                      Float ->
                      Float ->
                      ZRandom (GLfloat, GLfloat, GLfloat) -> 
                      (ZParticle -> ZRandomParticle) -> 
                      ZParticleEngine
zNewParticleEngine num seed pos dir base spread energy col step =
  ZParticleEngine { pEngF = step
                  , pEngNewP = zNewParticle base spread energy col
                  , pEngPs = ps
                  , pEngPos = pos
                  , pEngDir = dir
                  , pEngRand = rng }
  where newP      = zNewParticle base spread energy col pos dir
        rng0      = zRandGen seed
        (ps, rng) = zRunRand (sequence (replicate num newP)) rng0
        
zUpdateParticles :: Point3D Float -> Vector3D Float -> ZParticleEngine
                    -> ZParticleEngine
zUpdateParticles pos dir pe =
  pe { pEngPs = ps'
     , pEngRand = g'
     , pEngPos = pos
     , pEngDir = dir
     }
    where
      update p = case p of
        ZDeadParticle -> pEngNewP pe (pEngPos pe) (pEngDir pe)
        alive -> pEngF pe $ alive
      (ps', g') = zRunRand (mapM update (pEngPs pe)) (pEngRand pe)

zNewParticle :: Float -> Float -> Float ->
                ZRandom (GLfloat, GLfloat, GLfloat) ->
                Point3D Float ->
                Vector3D Float -> 
                ZRandomParticle
zNewParticle baseSpread theta str col or dir = do
  life <- toGLM $ zGetRandomR (0, 1)
  x <- zGetRandomR (-baseSpread, baseSpread)
  y <- zGetRandomR (-baseSpread, baseSpread)
  z <- zGetRandomR (-baseSpread, baseSpread)
  vy <- zGetRandomR (-cone, cone)
  vx <- zGetRandomR (-cone, cone)
  vz <- zGetRandomR (-cone, cone)
  color <- col
  return $ ZParticle { pLife  = life
                     , pColor = color
                     , pPos   = vecMap toGL $ vector3D (x,y,z) + or
                     , pVel   = vecMap toGL $ ZVector.scale str $ vector3D (vx,vy,vz) + dir
                     , pSize  = 1
                     }
    where
      toGLM = liftM toGL
      toGL  = realToFrac
      cone = str * sin theta

zStepParticle :: Double ->
                 ZParticle ->
                 ZRandomParticle
zStepParticle gravity p =
    let
        life' = pLife p - 0.2
        pos'  = pPos p + pVel p
        vel'  = pVel p - ZVector.scale (realToFrac gravity) yAxis
    in if life' > 0
       then return $ p { pLife = life'
                       , pPos = pos'
                       , pVel = vel'
                       }
       else return ZDeadParticle

renderParticles :: ZGraphicsResources -> ZParticleEngine -> IO ()
renderParticles res peng = do
    quadratic <- newArray [0, 0, 0.1]
    blend $= Enabled
    cullFace $= Nothing
    blendFunc $= (SrcAlpha, One)
    pointSprite $= Enabled
    pointSize $= 25
    depthMask $= Disabled
    textureBinding Texture2D $= Just (gTextures res ! 0)
    texture Texture2D $= Enabled
--    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
--    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
    glTexEnvi gl_POINT_SPRITE gl_COORD_REPLACE 1
    glPointParameterfv gl_POINT_DISTANCE_ATTENUATION quadratic
    lighting $= Disabled
    renderPrimitive Points $ do
      mapM_ (drawParticle res) (pEngPs peng)
    lighting $= Enabled
    texture Texture2D $= Disabled
    textureBinding Texture2D $= Nothing
    depthMask $= Enabled
    pointSprite $= Disabled
    blend $= Disabled
    
drawParticle _ ZDeadParticle = return ()
drawParticle _ p = do
  let (r,g,b) = pColor p
  pointSize $= (pLife p) * 25
  color $ Color4 r g b (pLife p)
  vertex (fromVec3D $ pPos p :: Vertex3 GLfloat)
