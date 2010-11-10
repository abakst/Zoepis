module ExampleObject(DT, ExampleObject(..), newShip, shipR) where

import ZScene
--import ZParticle
import ZGraphics
import ZVector
import ZRandom
import ZParticles

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
    , sEngineL :: ([ZParticle], Orientation)
    , sEngineR :: ([ZParticle], Orientation)
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
                 
shipR = do zLoadObject 0 "resources/GhoulOBJ.obj" True 
           zLoadTexture 0 "resources/Part.jpg"
           
newShip pos rot speed seed = mkShip $ Ship {
                                  sOrient = shipO
                                , sSpeed = speed
                                , sEngineL = eL
                                , sEngineR = eR
                                , sRand = zRandGen seed
                                }
    where shipO = newOrient pos rot
          eOff = scale 0.02 (oUp shipO)
          eRight = scale 0.35 (oRight shipO)
          eBack  = scale (-0.80) (oForward shipO)
          eL = (newEngine 25, offsetO (eOff - eRight + eBack) shipO)
          eR = (newEngine 25, offsetO (eOff + eRight + eBack) shipO)
           
mkShip :: Ship -> ExampleObject           
mkShip s = Object update render
    where
      transShip dt ship = moveShip ship (scale dt (shipVel ship))
      (rot, rng) = zRunRand (randomRot 0.5 (sOrient s)) (sRand s)
      stepShip sh dt = stepEngines $ transShip dt s -- $ rotateShip s rot
      update dt = mkShip $ (stepShip s dt) { sRand = rng }
      render = renderShip s
      
stepEngines :: Ship -> Ship
stepEngines ship = ship { sEngineL = stepEngine $ sEngineL ship
                        , sEngineR = stepEngine $ sEngineR ship
                        }
    where stepEngine (ps, o) = (updateAndSpawn o ps, o)
          updateAndSpawn o (ZDeadParticle:ps) =
              (engineParticle 1.0 o):(map justUpdate ps)
          updateAndSpawn o (p:ps) = (justUpdate p):(updateAndSpawn o ps)
          updateAndSpawn _ [] = []
          justUpdate ZDeadParticle = ZDeadParticle
          justUpdate p = if pLife p < 0
                         then ZDeadParticle
                         else p { pLife = pLife p - 0.1 }
          
newEngine n = replicate n $ ZDeadParticle          
          
engineParticle l o = particle l (0.1, 0.0, 0.9) (oPos o) (oForward o) 0
      
randomRot :: Float -> Orientation -> ZRandom (Quaternion (Float))
randomRot threshold orientation = do
  {
  ; p <- zGetRandomR (0, 1.0::Float)
  ; if p <= threshold
    then do
      theta <- zGetRandomR (0, 0.02)
      let rot = rotation theta (oForward orientation + oUp orientation)
      return rot
    else return $ rotation 0 (vector3D (0,0,0))
  }


renderShip :: Ship -> [ZSceneObject]
renderShip s = 
  [ZModel zNoScale (zRot $ rotate `mulq` reOrient) (zTrans pos) 0
  ,ZParticles peR
  ,ZParticles peL]
    where up = oUp $ sOrient s
          pos = oPos $ sOrient s
          peL = fst . sEngineL $ s
          peR = fst . sEngineR $ s
          reOrient = rotation (3*pi/2) yAxis `mulq` rotation (3*pi/2) xAxis
          rotate = oRotation $ sOrient s
