module Classess(Paint, paint,  
                Handle, handle,
                Tick, tick
                ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game(Event)
    
class Paint p where
    paint :: p -> IO Picture

instance Paint p => Paint [p] where
    paint [] = return blank 
    paint ps = do 
        paintings <- sequence $ map paint ps
        return $ pictures paintings

class Handle h where
    handle :: Event -> h -> IO h

instance Handle h => Handle [h] where 
    handle _ [] = return []
    handle e hs = sequence $ map (handle e) hs

class Tick t where
    tick :: Float -> t -> IO t

instance Tick h => Tick [h] where 
    tick f [] = return []
    tick f hs = sequence $ map (tick f) hs