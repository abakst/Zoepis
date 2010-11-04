module ExampleObject(DT, ExampleObject(..), newShip, shipR) where

import ZScene
import ZParticle
import ZGraphics
import ZVector
import ZRandom

import Control.Monad

type DT = Float

data ExampleObject = Object {
      objUpdate :: DT -> ExampleObject
    , objScene :: [ZSceneObject]
    }
                     
data Orientation = Orient {
  oRotation :: Quaternion Float
  , oCofR    :: Point3D Float  -- point about which to rotate
  , oOffset  :: Vector3D Float -- oCofR o + oOffset is the position
  } deriving Show
               
oUp o = rotateVector (oRotation o) yAxis
oForward o = rotateVector (oRotation o) zAxis
oRight o = rotateVector (oRotation o) (-xAxis)
                   
rotateO :: Quaternion Float -> Orientation -> Orientation                   
rotateO r o = o {
                oRotation = r `mulq` (oRotation o)
              , oOffset  = rot (oOffset o)
              }
    where rot v = rotateVector r v
          
offsetO :: Vector3D Float -> Orientation -> Orientation          
offsetO v o = o { oOffset = (oOffset o) + v }

transO :: Vector3D Float -> Orientation -> Orientation
transO v o = o { oCofR = (oCofR o) + v }
                   
oPos o = oCofR o + oOffset o                   

newOrient :: Point3D Float -> Quaternion Float  -> Orientation
newOrient pos rot = Orient { oCofR = pos
                           , oOffset = vector3D (0,0,0)
                           , oRotation = rot
                           }
  where up = rotateVector rot yAxis
        forward = rotateVector rot zAxis
                        
-- Ship specific, an example of an object --

data Ship = Ship {
      sOrient :: Orientation
    , sSpeed  :: Float
    , sEngineL :: (ZParticleEngine, Orientation)
    , sEngineR :: (ZParticleEngine, Orientation)
    , sRand    :: ZRandGen
    }
            
shipVel :: Ship -> Vector3D Float            
shipVel s = scale (sSpeed s) (oForward . sOrient $ s)

shipDir :: Ship -> Vector3D Float
shipDir = oForward . sOrient

moveShip :: Ship -> Vector3D Float -> Ship
moveShip s v = s { sOrient = transO v (sOrient s) 
                 , sEngineL = moveEngine (sEngineL s)
                 , sEngineR = moveEngine (sEngineR s)
                 }
    where moveEngine (pe, o) = (pe, transO v o)
          
rotateShip :: Ship -> Quaternion Float -> Ship          
rotateShip s r = s { sOrient = rotateO r (sOrient s)
                   , sEngineL = rotEngine (sEngineL s)
                   , sEngineR = rotEngine (sEngineR s)
                   }
    where rotEngine (pe, o) = (pe, rotateO r o)
                 
shipR = do zLoadObject 0 "resources/patrol.obj" True                       
           
newShip pos rot speed seed = mkShip $ Ship {
                                  sOrient = shipO
                                , sSpeed = speed
                                , sEngineL = eL
                                , sEngineR = eR
                                , sRand = zRandGen seed
                                }
    where shipO = newOrient pos rot
          engineO = shipO
          upOff = scale 0.27 (oUp shipO)
          upRight = scale 0.25    (oRight shipO)
          eL = newEngine (offsetO (upOff - upRight) shipO)
                         (return (0.3, 0.1, 0.6))
          eR = newEngine (offsetO (upOff + upRight) shipO)
                         (return (0.3, 0.1, 0.6))

           
mkShip :: Ship -> ExampleObject           
mkShip s = Object update render
    where
      stepEngine (pe, o) = (zUpdateParticles (oPos o) (scale (-1) $ oForward o) pe, o)
      transShip dt ship = moveShip ship (scale dt (shipVel ship))
{--}      
      stepEngines ship = ship { sEngineL = (stepEngine $ sEngineL ship)
                              , sEngineR = (stepEngine $ sEngineR ship)
                              }
--}
{-                          
      stepEngines = id
--}
      stepShip sh dt = stepEngines $ transShip dt sh
      update dt = mkShip $ stepShip s dt
      render = renderShip s
      
randomRot :: Float -> Orientation -> ZRandom (Quaternion (Float))
randomRot threshold orientation = do
  {
  ; p <- zGetRandomR (0, 1.0::Float)
  ; if p <= threshold
    then do
      theta <- zGetRandomR (0, 0.02)
      let rot = rotation theta (oForward orientation + oRight orientation)
      return rot
    else return $ rotation 0 (vector3D (0,0,0))
  }
      
newEngine o col =
  (zNewParticleEngine 25 1 (oPos o) (scale (-1) $ oForward o) 0.02 0.01 0.001 col up,o)
  where up = zStepParticle 0
        
renderShip :: Ship -> [ZSceneObject]
renderShip s = 
  [ZModel zNoScale (zRot rotation) (zTrans pos) 0
  ,ZParticles peR
  ,ZParticles peL]
    where up = oUp $ sOrient s
          pos = oPos $ sOrient s
          peL = fst . sEngineL $ s
          peR = fst . sEngineR $ s
          rotation = oRotation $ sOrient s
