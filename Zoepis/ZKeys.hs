module Zoepis.ZKeys where

import Data.Bits

newtype KeyVector = KV Integer

zAKey, zSKey, zDKey, zWKey :: Int    
zAKey = 0 
zSKey = 1
zDKey = 2 
zWKey = 3

zSetKey :: KeyVector -> Int -> KeyVector
zSetKey (KV k) i = KV $ k .|. bit i

zClearKey :: KeyVector -> Int -> KeyVector
zClearKey (KV k) i = KV $ k .&. complement (bit i)

zKeySet :: KeyVector -> Int -> Bool
zKeySet (KV k) = testBit k