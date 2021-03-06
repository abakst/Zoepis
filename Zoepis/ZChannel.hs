module Zoepis.ZChannel where

import Control.Concurrent.MVar

--- Channels are just wrappers around MVars ---                    
newtype ZChannel a = ZChannel (MVar a)

zNewChan a = fmap ZChannel (newMVar a)

zNewEmptyChan = fmap ZChannel newEmptyMVar
    
zPeekChan (ZChannel c) = readMVar c
                         
zTakeChan (ZChannel c) = takeMVar c
                         
zTryTakeChan (ZChannel c) = tryTakeMVar c
                            
zPutChan (ZChannel c)  = putMVar c
                         
zModifyChan_ (ZChannel c) = modifyMVar_ c

zSwapChan (ZChannel c) = swapMVar c

zIsEmpty (ZChannel c) = isEmptyMVar c
