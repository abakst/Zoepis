module Texture where

import Foreign
import Codec.Image.STB
import Data.ByteString ( unpack )
import Data.Bitmap
import Graphics.Rendering.OpenGL

bitmap2List = unpack . bitmapToByteString          
bitmap2Rows w = imageRows w . bitmap2List
withRows w f = concat . f . imageRows w
imageRows :: Int -> [a] -> [[a]]
imageRows w list = rows' list [[]]
    where rows' [] a = reverse a
          rows' list a = rows' (drop (3*w) list) (take (3*w) list:a)
          
withBitmap b act = do
    let dat =  withRows (snd $ bitmapSize b) reverse . bitmap2List $ b
    withArray dat $ act . PixelData RGB UnsignedByte
    
loadTexture :: String -> IO TextureObject
loadTexture image = do          
  px    <- loadImage image
  [tex] <- genObjectNames 1
  textureBinding Texture2D $= Just tex
  textureFunction $= Decal
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  case px of
    (Left s ) -> error s
    (Right b) -> withBitmap b . texify . bitmapSize $ b
  textureBinding Texture2D $= Nothing
  putStrLn $ show px
  return tex
      where 
        texify (w,h) = 
            texImage2D Nothing
                       NoProxy
                       0 
                       RGB'
                       (TextureSize2D (fromIntegral w) (fromIntegral h))
                       0
