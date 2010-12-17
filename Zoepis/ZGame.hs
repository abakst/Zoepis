{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Zoepis.ZGame where

import Zoepis.ZChannel
import Zoepis.ZEventMessage
import Zoepis.ZKeys

import Data.Monoid
import Control.Monad.State
import Control.Concurrent (yield)
import System.CPUTime

-- Time --

zTickPrec = 100000000
zGetTime  = getCPUTime >>= (return . (`div` zTickPrec))

-- Some handy classes to abstract common functionality --
class KeysEnabled a where            
  keKV      :: a -> KeyVector
  keSetKeys :: a -> KeyVector -> a
  
class Timed a where  
  tGetTicks :: a -> Integer
  tUpdateTicks :: a -> Integer -> a
  
-- Game Loop stuff --
type ZGameLoop st a = StateT st IO a
type ZHandlerFunc a st = a -> ZGameLoop st ()

instance Monoid (ZGameLoop st ()) where
  mempty = return ()
  mappend = (>>)

zGameLoop :: st -> ZGameLoop st () -> IO () -- loops forever
zGameLoop st loop = do
  st' <- zGameLoopStep st loop
  zGameLoop st' loop
  
zGameLoopStep :: st -> ZGameLoop st () -> IO st
zGameLoopStep st loop = st `seq` execStateT loop st

zEveryNTicks :: Timed st => Integer -> ZGameLoop st a -> ZGameLoop st ()
zEveryNTicks n act = do
  t0 <- gets tGetTicks
  t1 <- lift $ zGetTime
  when (t1 - t0 > n) $ loop t1 (t1 - t0)
  where loop t dif = if (dif > n)
                     then act >> loop t (dif - n) 
                     else modify $ flip tUpdateTicks t
                                          
zWithEventChannel :: ZChannel [ZEvent] -> ZEventHandler st -> ZGameLoop st ()
zWithEventChannel events hs =
    do maybeEs <- lift $ zTryTakeChan events
       case maybeEs of
         (Just es) -> do lift $ zPutChan events []
                         mapM_ applyHandlers es
         _ -> return ()
    where
      applyHandlers (KeyPress x) = zKeyPress hs x
      applyHandlers (KeyRelease x) = zKeyRelease hs x
      applyHandlers (MouseUp b) = zMouseUp hs b
      applyHandlers (MouseDown b (x,y)) =
          zMouseDown hs (b, (fromIntegral x, fromIntegral y))
      applyHandlers (MouseMove (x,y)) =
          zMouseMove hs (fromIntegral x, fromIntegral y)
      applyHandlers (Tic t) = zTic hs t
                                  
data ZEventHandler st = ZEventHandler {
      zKeyPress   :: ZHandlerFunc Char st
    , zKeyRelease :: ZHandlerFunc Char st
    , zMouseDown  :: ZHandlerFunc (Button, (Int, Int)) st
    , zMouseUp    :: ZHandlerFunc Button st
    , zMouseMove  :: ZHandlerFunc (Int, Int) st
    , zTic        :: ZHandlerFunc Int st
    }
                        
class EventSub a where                        
  esEventChannel :: a -> ZChannel [ZEvent]
                        
zIgnoreEvent :: ZHandlerFunc a st                     
zIgnoreEvent = mempty
                        
zEmptyHandler = ZEventHandler {
                  zKeyPress = zIgnoreEvent
                , zKeyRelease = zIgnoreEvent
                , zMouseDown = zIgnoreEvent
                , zMouseUp = zIgnoreEvent
                , zMouseMove = zIgnoreEvent
                , zTic = zIgnoreEvent
               }
                
zHandleEvents :: EventSub st => ZEventHandler st -> ZGameLoop st ()
zHandleEvents h = gets esEventChannel >>= flip zWithEventChannel h
  
bothAppend :: Monoid m => (a -> m) -> a -> a -> m
bothAppend f r1 r2 = f r1 `mappend` f r2

(>+<) :: ZEventHandler st -> ZEventHandler st -> ZEventHandler st
eh1 >+< eh2 = ZEventHandler {
                zKeyPress = combine zKeyPress
              , zKeyRelease = combine zKeyRelease
              , zMouseDown = combine zMouseDown
              , zMouseUp = combine zMouseUp
              , zMouseMove = combine zMouseMove
              , zTic = combine zTic
              }
    where combine h = h eh1 `mappend` h eh2
