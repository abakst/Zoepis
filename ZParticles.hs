module ZParticles where

import ZVector
import ZGraphics

import Data.Map
import Unsafe.Coerce
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL

toGL :: Float -> GLfloat
toGL = unsafeCoerce

data ZParticle = ZDeadParticle
               | ZParticle { 
                   pLife :: GLfloat
                 , pColor ::(GLfloat, GLfloat, GLfloat)
                 , pPos  :: Point3D GLfloat
                 , pDir  :: Vector3D GLfloat
                 , pSpeed :: GLfloat
                 }
                 
particle l (r,g,b) p d s =
    ZParticle {
  pLife = toGL l
, pColor = (toGL r, toGL g, toGL b)
, pPos = vecMap toGL p
, pDir = vecMap toGL d
, pSpeed = toGL s
}
                 
renderParticles res ps = do
  quadratic <- newArray [0, 0, 0.1]
  blend $= Enabled
  cullFace $= Nothing
  blendFunc $= (SrcAlpha, One)
  pointSprite $= Enabled
  pointSize $= 25
  depthMask $= Disabled
  textureBinding Texture2D $= Just (gTextures res ! 0)
  texture Texture2D $= Enabled
  glTexEnvi gl_POINT_SPRITE gl_COORD_REPLACE 1
  glPointParameterfv gl_POINT_DISTANCE_ATTENUATION quadratic
  lighting $= Disabled
  renderPrimitive Points $ mapM_ (drawParticle res) ps
  lighting $= Enabled
  texture Texture2D $= Disabled
  textureBinding Texture2D $= Nothing
  depthMask $= Enabled
  pointSprite $= Disabled
  blend $= Disabled

drawParticle _ ZDeadParticle = return ()        
drawParticle _ p = do    
    let (r,g,b) = pColor p
--    pointSize $= (pLife p) * 25
    color $ Color4 r g b (pLife p)
    vertex (fromVec3D $ pPos p :: Vertex3 GLfloat)