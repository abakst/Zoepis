module Zoepis.ZParticles where

import Zoepis.ZVector
import Zoepis.ZGraphics

import Data.Map
import Unsafe.Coerce
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.Raw (gl_POINT_SPRITE, 
				      gl_COORD_REPLACE,
                                      gl_POINT_DISTANCE_ATTENUATION,
				      glTexEnvi, glPointParameterfv)
import Graphics.Rendering.OpenGL.GL

data ZParticle = ZDeadParticle
               | ZParticle { 
                   pLife :: GLfloat
                 , pColor ::(GLfloat, GLfloat, GLfloat)
                 , pPos  :: Point3D GLfloat
                 , pDir  :: Vector3D GLfloat
                 , pSpeed :: GLfloat
                 }

zParticle :: Float -> (Float, Float, Float) ->
             Point3D Float -> Vector3D Float -> Float ->
             ZParticle
zParticle l (r,g,b) p d s =
    ZParticle {
  pLife = toGL l
, pColor = (toGL r, toGL g, toGL b)
, pPos = vecMap toGL p
, pDir = vecMap toGL d
, pSpeed = toGL s
}
                 
zRenderParticles :: ZGraphicsResources -> Float -> Int -> [ZParticle] -> IO ()
zRenderParticles res size tex ps = do
  quadratic <- newArray [0, 0, 0.1]
  blend $= Enabled
  cullFace $= Nothing
  blendFunc $= (SrcAlpha, One)
  pointSprite $= Enabled
  pointSize $= toGL size
  depthMask $= Disabled
  textureBinding Texture2D $= Just (gTextures res ! tex)
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
    color $ Color4 r g b (pLife p)
    vertex (fromVec3D $ pPos p :: Vertex3 GLfloat)
