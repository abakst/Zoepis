{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies #-}
module Zoepis.ZGraphics where

import Zoepis.ZObject
import Zoepis.ZChannel
import Zoepis.ZEventMessage
import Zoepis.Texture
import Control.Concurrent (yield, forkOS, forkIO)
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (get, LeftButton)
import Data.Map
import Control.Monad.State as S
import Data.IORef
import Data.List (sortBy)
import System.Exit
import Data.Maybe

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
                     
runWithGraphics startGr act = forkIO act >> startGr

zUpdateGraphics :: IO ()
zUpdateGraphics = postRedisplay Nothing --
                  
-- I'll add exception handling one day --
zLoadObject :: Int -> String -> Bool -> ZResourceLoader
zLoadObject id str unit = do
  res <- S.get
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
    , gWindows  :: ZWindowGL a
    }
                     
data ZWindowGL a = ZWindowGL {
      gWindowName :: String
    , gWidth  :: !GLsizei
    , gHeight :: !GLsizei
    , gWindowOffset :: !(GLint, GLint)
    , gSceneChannel :: !(ZChannel a)
    , gEventChannel :: !(Maybe (ZChannel [ZEvent]))
    , gSubWindows :: [ZWindowGL a]
    , gResloader  :: ZResourceLoader
    , gWindowChannel :: (ZChannel [ZWindowMessage])
    }
                   
data ZWindowMessage = HideWindow
                    | ShowWindow
                    | PositionWindow (GLint, GLint)
                    | SizeWindow (GLsizei, GLsizei)                   
                      deriving Show

handleWMsgs eventC = do
  events <- zTakeChan eventC
  zPutChan eventC []
  when (length events > 0) $ do
                       mapM_ applyWindowMsg events
                       postRedisplay Nothing
  
applyWindowMsg HideWindow = windowStatus $= Hidden
applyWindowMsg ShowWindow = windowStatus $= Shown
applyWindowMsg (PositionWindow (x, y)) = windowPosition $= Position x y
applyWindowMsg (SizeWindow (w, h)) = windowSize $= Size w h
                   
class GraphicsEnabled a b | a -> b where                     
  geGraphics :: a -> ZGraphicsGL b                      
    
zModifyScene chan f = zModifyChan_ chan (return . f)
        
setupWindows :: ZRenderGL a => Maybe Window -> ZWindowGL a ->
                IO [(Window, ZChannel [ZWindowMessage])]
setupWindows parent w = do
  let (wi, he) = (gWidth w, gHeight w)
  let (x,y) = gWindowOffset w
  let scene = gSceneChannel w
  let event = gEventChannel w
  let pos = Position x y
  let size = Size wi he
  wHandle <- (if isJust parent
              then createSubWindow (fromJust parent) pos size
              else createWindow (gWindowName w))
  currentWindow $= Just wHandle
  -- We need to load resources per-window --
  res <- execStateT (gResloader w) zEmptyResourceList
  -- Set up basic stuff --
  displayCallback $= display wHandle w res scene
  eventSetup wHandle (gWindowChannel w) event
  basicSetup wi he
  windowStatus $= Shown
  eventChannels <- mapM (setupWindows (Just wHandle)) $ gSubWindows w
  -- In case this changed, set the currentWindow again --
  currentWindow $= Just wHandle
  return (concat eventChannels ++ [(wHandle, gWindowChannel w)])

display wptr wobj res sceneChan = do
  scene <- zTakeChan sceneChan
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  zRenderGL res scene
  zPutChan sceneChan scene
  flush
  postRedisplay Nothing

eventSetup wHandle msgC (Just events) = do
  motionCallback $= Just (mouseCallback  events)
  keyboardMouseCallback $= Just (\a b c d -> do
                                   cw <- GL.get currentWindow
                                   currentWindow $= Just wHandle
                                   handleWMsgs msgC
                                   currentWindow $= cw
                                   buttonCallback events a b c d)
eventSetup _ _ _ = return ()    

idle events = do
  cw <- GL.get currentWindow
  mapM_ getMsgs events
  currentWindow $= cw
    where getMsgs (handle, channel) = do
                currentWindow $= Just handle
                handleWMsgs channel
  
zInitialize :: ZRenderGL scene =>
               String ->
               ZWindowGL scene -> 
               ZChannel Bool -> 
               IO (IO () -> IO ())
zInitialize name windows exit = do
  let initGL = startGL windows exit
  return $ runWithGraphics initGL
        where
          startGL windows exitVar = do
            --- Basic init ---
            initialize name []
            initialDisplayMode $= [SingleBuffered, RGBMode, WithDepthBuffer]
            let (width, height) = (gWidth windows, gHeight windows)
            initialWindowSize $= Size width height
            -- Stuff --
            events <- setupWindows Nothing windows
            idleCallback $= (Just $ do quit <- zTryTakeChan exitVar
                                       when (isJust quit) $ do
                                         exitWith ExitSuccess
                                       idle events
                                       return ())
            basicSetup width height
            fullScreen
            mainLoop
            
basicSetup width height = do            
  matrixMode $= Projection
  loadIdentity
  perspective 45 
             (fromIntegral width / fromIntegral height)
             0.01
             500
  matrixMode $= Modelview 0
  depthFunc $= Just Less
  hint PointSmooth $= Nicest
  shadeModel $= Smooth
  cullFace $= Nothing
  clearColor $= Color4 0.1 0.1 0.1 0.0
  clearDepth $= 1.0
  normalize $= Enabled
  setupLights

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
     shadeModel $= Smooth
     lighting $= Enabled
