{-# LANGUAGE TypeSynonymInstances #-}
module ZGame where

import Data.Monoid
import Control.Monad.State
import Control.Concurrent (yield)
import ZChannel
import ZEventMessage

type ZGameLoop st = StateT st IO ()
type ZHandlerFunc a st = a -> ZGameLoop st

instance Monoid (ZGameLoop st) where
  mempty = return ()
  mappend = (>>)

zWithEventChannel :: ZChannel [ZEvent] -> ZEventHandler st -> ZGameLoop st
zWithEventChannel events hs =
    do maybeEs <- lift $ zTryTakeChan events
       case maybeEs of
         (Just es) -> do lift $ zPutChan events []
                         mapM_ applyHandlers (reverse es)
         _ -> lift yield
    where
      applyHandlers (KeyPress x) = zKeyPress hs x
      applyHandlers (KeyRelease x) = zKeyRelease hs x
      applyHandlers (MouseUp b) = zMouseUp hs b
      applyHandlers (MouseDown b (x,y)) =
          zMouseDown hs (b, (fromIntegral x, fromIntegral y))
      applyHandlers (MouseMove (x,y)) =
          zMouseMove hs (fromIntegral x, fromIntegral y)
                                  
data ZEventHandler st = ZEventHandler {
      zKeyPress   :: ZHandlerFunc Char st
    , zKeyRelease :: ZHandlerFunc Char st
    , zMouseDown  :: ZHandlerFunc (Button, (Int, Int)) st
    , zMouseUp    :: ZHandlerFunc Button st
    , zMouseMove  :: ZHandlerFunc (Int, Int) st
    }
                        
zIgnoreEvent :: ZHandlerFunc a st                     
zIgnoreEvent = mempty
                        
zEmptyHandler = ZEventHandler {
                 zKeyPress = zIgnoreEvent
               , zKeyRelease = zIgnoreEvent
               , zMouseDown = zIgnoreEvent
               , zMouseUp = zIgnoreEvent
               , zMouseMove = zIgnoreEvent
               }

bothAppend :: Monoid m => (a -> m) -> a -> a -> m
bothAppend f r1 r2 = f r1 `mappend` f r2

(>+<) :: ZEventHandler st -> ZEventHandler st -> ZEventHandler st
eh1 >+< eh2 = ZEventHandler {
        zKeyPress = combine zKeyPress
      , zKeyRelease = combine zKeyRelease
      , zMouseDown = combine zMouseDown
      , zMouseUp = combine zMouseUp
      , zMouseMove = combine zMouseMove
      }
    where combine h = h eh1 `mappend` h eh2
