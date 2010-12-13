module Zoepis.ZGraphics where

import Zoepis.ZObject
import Zoepis.ZChannel
import Zoepis.ZEventMessage
import Zoepis.Texture
import Control.Concurrent (yield, forkOS, forkIO)
import Graphics.Rendering.OpenGL hiding (get)
import Graphics.UI.GLUT hiding (get, LeftButton)
import Data.Map
import Control.Monad.State
import Data.IORef
import Data.List (sortBy)
import System.Exit

--- Resources ---
data ZGraphicsResources = GraphicsResources {
      gDisplayLists :: (Map Int ZObject)
    , gTextures     :: (Map Int TextureObject)
    }
                          
type ZResourceLoader = StateT ZGraphicsResources IO ()

zEmptyResourceList = GraphicsResources {
                       gDisplayLists = empty
                     , gTextures = empty
                     }
                     
zRunWithGraphics startGr gr act = do
  forkIO $ do
    x <- zTakeChan (gEventChannel gr)
    zPutChan (gEventChannel gr) []
    act
  startGr

zUpdateGraphics :: IO ()
zUpdateGraphics = postRedisplay Nothing  >> yield
                  
-- I'll add exception handling one day --
zLoadObject :: Int -> String -> Bool -> ZResourceLoader
zLoadObject id str unit = do
  res <- get
  obj <- lift $ zLoadObjectFile str unit
  let newres = insert id obj (gDisplayLists res)
  put $ res { gDisplayLists = newres }
  return ()                     

zLoadTexture :: Int -> String -> ZResourceLoader
zLoadTexture id str = do
  l <- lift $ loadTexture str
  modify (\res -> res { gTextures = insert id l (gTextures res) })
  
--- Definition of the class of renderable scenes ---
class ZRenderGL a where                     
    zRenderGL :: ZGraphicsResources -> a -> IO ()                     
    
data NullScene = NullScene -- for basic init testing                     
instance ZRenderGL NullScene where
    zRenderGL _ _ = return ()

--- Initialization/Setup ---                
data ZGraphicsGL a = GraphicsGL {
      gProgname :: !String
    , gWidth    :: !GLsizei
    , gHeight   :: !GLsizei
    , gSceneChannel :: !(ZChannel a)
    , gEventChannel :: !(ZChannel [ZEvent])
    }
                     
zInitialize :: ZRenderGL scene =>
               String ->
               GLsizei ->
               GLsizei ->
               ZResourceLoader ->
               scene ->
               ZChannel Bool -> 
               IO (ZGraphicsGL scene, IO ())
zInitialize name width height loader startScene exit =
    do sceneVar <- zNewChan startScene
       eventVar <- zNewEmptyChan
       return (GraphicsGL {
                    gProgname = name
                  , gWidth = width
                  , gHeight = height
                  , gSceneChannel = sceneVar
                  , gEventChannel = eventVar
                  }, startGL loader sceneVar eventVar exit)
        where
          display res sceneChan = do
            scene <- zPeekChan sceneChan
            clear [ ColorBuffer, DepthBuffer ]
            loadIdentity
            zRenderGL res scene
            flush
            yield
          startGL resloader sceneVar eventVar exitVar = do
            --- Basic init ---
            initialize name []
            initialDisplayMode $= [SingleBuffered, RGBMode, WithDepthBuffer]
            initialWindowSize $= Size width height
            createWindow name
--	    fullScreen
            res <- execStateT resloader zEmptyResourceList
            displayCallback $= display res sceneVar
            idleCallback $= (Just $ do x <- zIsEmpty exitVar
                                       when (not x) exitSuccess
                                       yield)
            motionCallback  $= Just (mouseCallback eventVar)
            keyboardMouseCallback $= Just (buttonCallback eventVar)
            --- Other stuff now ---
            matrixMode $= Projection
            loadIdentity
            perspective 45 
                        (fromIntegral width / fromIntegral height)
                        0.1
                        200
            matrixMode $= Modelview 0
            depthFunc $= Just Less
            hint PointSmooth $= Nicest
            shadeModel $= Smooth
            cullFace $= Nothing
            clearColor $= Color4 0.1 0.1 0.1 0.0
            clearDepth $= 1.0
            normalize $= Enabled
            setupLights
	    zPutChan eventVar []
            mainLoop

setupLights = do 
  lighting              $= Enabled
  light (Light 0)       $= Enabled
  light (Light 1)       $= Enabled
  lightModelAmbient     $= Color4 1.5 1.5 1.5 1
  lightModelLocalViewer $= Enabled
  position (Light 0)    $= Vertex4 1 2 1 (1::GLfloat)
  position (Light 1)    $= Vertex4 (-1.0) 2 (-1.0) (1::GLfloat)
           

sendEvent :: ZEvent -> ZChannel [ZEvent] -> IO ()  
sendEvent e chan = zModifyChan_ chan (\es -> return (e:es))  >> yield

mouseCallback :: ZChannel [ZEvent] -> MotionCallback
mouseCallback chan (Position x y) = let e = MouseMove (x, y)
                                    in sendEvent e chan
                                       
buttonCallback :: ZChannel [ZEvent] -> KeyboardMouseCallback
buttonCallback chan (MouseButton _) Down _ (Position x y) =
    let e = MouseDown LeftButton (x, y) in sendEvent e chan
buttonCallback chan (Char c) st _ _ = let e = case st of
                                                Up -> KeyRelease
                                                Down -> KeyPress
                                      in sendEvent (e c) chan
buttonCallback _ _ _ _ _ = return ()

drawAxes = do
     lighting $= Disabled
     shadeModel $= Flat
     currentColor $= Color4 1 0 0 1
     renderPrimitive Lines $ do
                vertex (Vertex3 0.0 0.0 0.0::Vertex3 GLfloat)
                vertex (Vertex3 1.0 0.0 0.0::Vertex3 GLfloat)
     currentColor $= Color4 0 1 0 1
     renderPrimitive Lines $ do
                vertex (Vertex3 0.0 0.0 0.0::Vertex3 GLfloat)
                vertex (Vertex3 0.0 1.0 0.0::Vertex3 GLfloat)      
     currentColor $= Color4 0 0 1 1
     renderPrimitive Lines $ do
                vertex (Vertex3 0.0 0.0 0.0::Vertex3 GLfloat)
                vertex (Vertex3 0.0 0.0 1.0::Vertex3 GLfloat)
     shadeModel $= Flat
     lighting $= Enabled
