module ZVector where

import Data.SG hiding (origin)
import Control.Applicative
import Debug.Trace
import Graphics.Rendering.OpenGL

debug x = trace (show x) x

type Vector3D a = Rel3' a
type Point3D  a = Vector3D a

vecMap :: Num b => (a -> b) -> Vector3D a -> Vector3D b 
vecMap = fmapNum1    

data Quaternion a = Quaternion a a a a deriving (Show, Read)

instance Functor Quaternion where             
  fmap f (Quaternion w x y z) = Quaternion (f w) (f x) (f y) (f z)
  
instance Applicative Quaternion where  
  pure x = Quaternion x x x x
  (Quaternion f1 f2 f3 f4) <*> (Quaternion w x y z) =
      Quaternion (f1 w) (f2 x) (f3 y) (f4 z)
      
quaternion = Quaternion

real :: Quaternion a -> a
real (Quaternion r _ _ _) = r

imaginary :: Quaternion a -> (a, a, a)
imaginary (Quaternion _ x y z) = (x, y, z)

conj :: Num a => Quaternion a -> Quaternion a
conj (Quaternion w x y z) = Quaternion w (negate x) (negate y) (negate z)

qmag :: Floating a => Quaternion a -> a
qmag = sqrt . qmagsq

qmagsq :: Num a => Quaternion a -> a
qmagsq (Quaternion a b c d) = a*a + b*b + c*c + d*d

inverse :: Fractional a => Quaternion a -> Quaternion a
inverse q = fmap (/(qmagsq q)) (conj q)
          
dotq :: Num a => Quaternion a -> Quaternion a -> a
dotq p q = let (Quaternion a b c d) = (*) <$> p <*> q
           in  a + b + c + d

mulq :: (Floating a, Ord a) => Quaternion a -> Quaternion a -> Quaternion a
mulq p q = quaternion (rsps - pdotq) a b c
    where
      rsps  = real p * real q
      pdotq = vectorq p `dotProduct` vectorq q
      pxq   = vectorq p `cross` vectorq q
      pqv   = real p `scaleRel` vectorq q
      pvq   = real q `scaleRel` vectorq p
      (Rel3 (a,b,c) _) = pxq + pqv + pvq
          
cross :: Num a => Vector3D a -> Vector3D a -> Vector3D a
cross (Rel3 (a1,a2,a3) _) (Rel3 (b1,b2,b3) _) =          
    vector3D (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)
    
vectorq :: Num a => Quaternion a -> Vector3D a
vectorq q = vector3D $ imaginary q

axisOfRot :: (Ord a, Floating a) => Quaternion a -> Vector3D a
axisOfRot = unitVector . vectorq
                        
angleOfRot :: Floating a => Quaternion a -> a
angleOfRot = (2*) . acos . real

noRotation = rotation 0 (vector3D (0,0,0))

rotation :: (Floating a, Ord a) => a -> Vector3D a -> Quaternion a
rotation alpha axis = Quaternion (cos(alpha/2)) x y z
    where
      (Rel3 (x,y,z) _) = fmapNum1 (sin(alpha/2)*) (unitVector axis)
      
rotateVector :: (Floating a, Ord a) => Quaternion a -> Vector3D a -> Vector3D a
rotateVector q v = vectorq $ q `mulq` vecq `mulq` inverse q
    where
      vecq = Quaternion 0 (vecX v) (vecY v) (vecZ v)
      
rotateTo :: (Floating a, Ord a) =>
            Vector3D a ->
            Vector3D a ->
            Maybe (Vector3D a) ->
            Quaternion a
rotateTo v d u = case u of
                   Nothing -> q
                   Just up -> rotation phi d `mulq` q
                       where
                         norm  = d `cross` up
                         dp    = rotateVector q up `dotProduct` norm
                         phi   = acos dp - pi/2
    where 
      axis  = v `cross` d
      theta = acos (v `dotProduct` d)
      q     = rotation theta axis
          
vector3D :: Num a => (a, a, a) -> Vector3D a
vector3D = makeRel3

point3D :: Num a => (a, a, a) -> Vector3D a
point3D = makeRel3

scale :: (Num a) =>  a -> Vector3D a -> Vector3D a
scale s = fmapNum1 (*s)

xAxis :: Num a => Vector3D a
xAxis = vector3D (1, 0, 0)

yAxis :: Num a => Vector3D a
yAxis = vector3D (0, 1, 0)

zAxis :: Num a => Vector3D a
zAxis = vector3D (0, 0, 1)

origin :: Num a => Vector3D a         
origin = vector3D (0, 0, 0)

vecX = Data.SG.getX
vecY = Data.SG.getY
vecZ = Data.SG.getZ

unit :: (Ord a, Floating a) => Vector3D a -> Vector3D a
unit = Data.SG.unitVector

zeroV :: Num a => Vector3D a
zeroV = vector3D (0, 0, 0)

toDeg = (*(180/pi))

zDot :: Floating a => Vector3D a -> Vector3D a -> a
zDot = dotProduct

zCross :: Floating a => Vector3D a -> Vector3D a -> Vector3D a
zCross = cross

zMagSq :: Floating a => Vector3D a -> a
zMagSq = magSq

class IsoVecTo a where    
    fromVec3D :: Vector3D f -> a f
    
instance IsoVecTo Vector3 where
    fromVec3D v = Vector3 (vecX v) (vecY v) (vecZ v)
    
instance IsoVecTo Vertex3 where
    fromVec3D v = Vertex3 (vecX v) (vecY v) (vecZ v)    
  