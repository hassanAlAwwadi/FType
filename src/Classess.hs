module Classess(
    Paint, paint, PaintIO, paintIO,
    Handle, handle, HandleIO, handleIO,
    Tick, tick, TickIO, tickIO,
    Collidable, size , position, repos, reposChildren, reposWithChildren, checkCollision, outOfBorder, pastBorders,
    Creatable, create
) where

import Resources
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game(Event)
    
class Paint p where
    paint :: p -> Picture

class PaintIO p where
    paintIO :: p -> IO Picture

instance Paint p => Paint [p] where
    paint [] = blank 
    paint ps = pictures $ map paint ps

instance Paint Picture where
    paint = id

class Handle h where
    handle :: Event -> h -> h

instance Handle h => Handle [h] where 
    handle _ [] = []
    handle e hs = map (handle e) hs

class HandleIO h where
    handleIO :: Event -> h -> IO h

class Tick t where
    tick :: Float -> t -> t

instance Tick h => Tick [h] where 
    tick _ [] = []
    tick f hs = map (tick f) hs

class TickIO t where
    tickIO :: Float -> t -> IO t

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

outOfBorder :: Collidable a => Border -> a -> Bool
outOfBorder b a = let
    (x,y) = position a
    in (x < xmin b || x > xmax b || y < ymin b || y > ymax b)


pastBorders :: Collidable a => Border -> a -> Bool
pastBorders b a = let
    (x,_) = position a
    in (x < xmin b)

class Creatable c where
    create :: StaticResource -> DynamicResource -> c