module Classess(
    Paint, paint,  
    Handle, handle,
    Tick, tick,
    Collidable, size , position, repos, reposChildren, reposWithChildren,
    checkCollision
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game(Event)
    
class Paint p where
    paint :: p -> IO Picture

instance Paint p => Paint [p] where
    paint [] = return blank 
    paint ps = do 
        paintings <- mapM paint ps
        return $ pictures paintings
        
instance Paint p => Paint (IO p) where
    paint iop = iop >>= paint

instance Paint Picture where
    paint = pure

class Handle h where
    handle :: Event -> h -> IO h

instance Handle h => Handle [h] where 
    handle _ [] = return []
    handle e hs = mapM (handle e) hs

class Tick t where
    tick :: Float -> t -> IO t

instance Tick h => Tick [h] where 
    tick _ [] = return []
    tick f hs = mapM (tick f) hs

class Collidable c where
    size :: c -> (Float,Float)
    position :: c -> Point
    repos :: Vector -> c -> c
    reposChildren :: Vector -> c -> c

reposWithChildren :: Collidable c => Vector -> c -> c
reposWithChildren v = repos v . reposChildren v 
 
checkCollision :: (Collidable a,Collidable b) => a -> b -> Bool
checkCollision c1 c2= not (intersectX (size c1) (position c1) (size c2) (position c2) || intersectY (size c1) (position c1) (size c2) (position c2))
    where   intersectX    (xs1,_) (xp1,_) (xs2,_) (xp2,_) |xp2>xp1=xp2 - (xp1 + xs1/2 + xs2/2) > 0 
                                                          |xp2<xp1=xp1 - (xp2 + xs1/2 + xs2/2) > 0
                                                          |otherwise = False
            intersectY    (_,ys1) (_,yp1) (_,ys2) (_,yp2) |yp2>yp1=yp2 - yp1 - ys1/2 - ys2/2 > 0 
                                                          |yp2<yp1=yp1 - (yp2 + ys1/2 + ys2/2) > 0
                                                          |otherwise = False