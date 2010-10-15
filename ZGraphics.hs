module ZGraphics where

import Control.Concurrent (ThreadId, forkIO, yield)
--import Control.Concurrent.MVar    
import Graphics.Rendering.OpenGL hiding (get)
import Graphics.UI.GLUT hiding (get, LeftButton)
import Data.Map
import Control.Monad.State
import Data.IORef

import ZObject
import ZChannel
import ZEventMessage

--- Resources ---
data ZGraphicsResources = GraphicsResources {
      gDisplayLists :: (Map Int ZObject)
    }
                          
type ZResourceLoader = StateT ZGraphicsResources IO ()

zEmptyResourceList = GraphicsResources {
                       gDisplayLists = empty
                     }
                     
zUpdateGraphics = postRedisplay Nothing >> yield                     
-- I'll add exception handling one day --
zLoadObject :: Int -> String -> ZResourceLoader
zLoadObject id str = do
  res <- get
  obj <- lift $ zLoadObjectFile str
  let newres = insert id obj (gDisplayLists res)
  put $ res { gDisplayLists = newres }
  return ()                     

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
    , gThreadId :: !ThreadId
    , gSceneChannel :: !(ZChannel a)
    , gEventChannel :: !(ZChannel [ZEvent])
    }
                     
zInitialize :: ZRenderGL scene =>
               String ->
               GLsizei ->
               GLsizei ->
               scene ->
               ZResourceLoader ->
               Int ->
               IO (ZGraphicsGL scene)
zInitialize name width height scene loader period =
    do sceneVar <- zNewChan scene
       eventVar <- zNewChan []
       tickgen eventVar period
       threadId <- forkIO $ startGL loader sceneVar eventVar
       return GraphicsGL {
                    gProgname = name
                  , gWidth = width
                  , gHeight = height
                  , gThreadId = threadId
                  , gSceneChannel = sceneVar
                  , gEventChannel = eventVar
                  }
        where
          tickgen eChan p =
              addTimerCallback p $ sendEvent (Tic p) eChan >> tickgen eChan p
          display res sceneChan = do
            scene <- zPeekChan sceneChan
            clear [ ColorBuffer, DepthBuffer ]
            loadIdentity
            zRenderGL res scene
            flush
          startGL resloader sceneVar eventVar = do
            --- Basic init ---
            initialize name []
            initialDisplayMode $= [SingleBuffered, RGBMode, WithDepthBuffer]
            initialWindowSize $= Size width height
            createWindow name
            res <- execStateT resloader zEmptyResourceList
            displayCallback $= display res sceneVar
            motionCallback  $= Just (mouseCallback eventVar)
            keyboardMouseCallback $= Just (buttonCallback eventVar)
            --- Other stuff now ---
            matrixMode $= Projection
            loadIdentity
            perspective 45 
                        (fromIntegral width / fromIntegral height)
                        0.1
                        50
            matrixMode $= Modelview 0
            depthFunc $= Just Less
            hint PointSmooth $= Nicest
            shadeModel $= Smooth
            cullFace $= Nothing
            clearColor $= Color4 0 0 0 0
            clearDepth $= 1.0
            normalize $= Enabled
            setupLights
            mainLoop
                
setupLights = do 
  lighting              $= Enabled
  light (Light 0)       $= Enabled
  light (Light 1)       $= Enabled
  lightModelAmbient     $= Color4 0.4 0.4 0.4 1
  lightModelLocalViewer $= Disabled
  position (Light 0)    $= Vertex4 1 2 1 (1::GLfloat)
  position (Light 1)    $= Vertex4 (-1.0) 2 (-1.0) (1::GLfloat)                
  
sendEvent :: ZEvent -> ZChannel [ZEvent] -> IO ()  
sendEvent e chan = zModifyChan_ chan (\es -> return (e:es))

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