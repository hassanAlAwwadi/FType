module Ship(Ship, bullets,Ship.powerUp, pos, p1c, p2c, controls) where

import qualified Classess as C
import Resources(Border, playerShip, border)
import Graphics.Gloss
import Weapon as W(Gun,Bullet, PowerUp, simpleShip, shoot, powerUp) 
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as L


data Ship = Ship {
    pos :: Point, 
    speed :: Float,
    direction :: Vector,
    gun :: Gun, 
    bullets :: [Bullet],
    bombs :: Int,
    size :: (Float,Float),
    borders :: Border,
    controls :: Event -> Ship -> Ship,
    anim :: [Picture]
} 

p1c :: Event -> Ship -> Ship
p1c e s@Ship{direction = (xd,yd)} = case e of   
    (EventKey (Char 'w') Down _ _) -> s { direction = ( xd,  1 ) }
    (EventKey (Char 's') Down _ _) -> s { direction = ( xd, -1 ) }
    (EventKey (Char 'd') Down _ _) -> s { direction = (  1, yd ) }
    (EventKey (Char 'a') Down _ _) -> s { direction = ( -1, yd ) }
    (EventKey (Char 'w') Up   _ _) -> s { direction = ( xd,  0 ) }
    (EventKey (Char 's') Up   _ _) -> s { direction = ( xd,  0 ) }
    (EventKey (Char 'd') Up   _ _) -> s { direction = (  0, yd ) }
    (EventKey (Char 'a') Up   _ _) -> s { direction = (  0, yd)  }
    _                          -> s

p2c :: Event -> Ship -> Ship
p2c e s@Ship{direction = (xd,yd)} = case e of   
    (EventKey (Char 'i') Down _ _) -> s { direction = ( xd,  1 ) }
    (EventKey (Char 'k') Down _ _) -> s { direction = ( xd, -1 ) }
    (EventKey (Char 'l') Down _ _) -> s { direction = (  1, yd ) }
    (EventKey (Char 'j') Down _ _) -> s { direction = ( -1, yd ) }
    (EventKey (Char 'i') Up   _ _) -> s { direction = ( xd,  0 ) }
    (EventKey (Char 'k') Up   _ _) -> s { direction = ( xd,  0 ) }
    (EventKey (Char 'l') Up   _ _) -> s { direction = (  0, yd ) }
    (EventKey (Char 'j') Up   _ _) -> s { direction = (  0, yd)  }
    _                          -> s

instance C.Creatable Ship where
    create s _ = Ship{ 
        pos = (-420,0), 
        speed = 13, 
        direction = (0,0), 
        gun = simpleShip, 
        bullets = [], 
        bombs = 0, 
        size = (40,20),
        borders = border s,
        controls = p1c,
        anim = cycle $ playerShip s
    } 

powerUp :: Ship -> PowerUp -> Ship
powerUp s p = s{ gun = W.powerUp (gun s) p}

instance C.Paint Ship where
    paint Ship{ pos = (x,y), bullets = b, size = (sx,sy), anim = as} =  
        let pb = C.paint b
            p = case as of a:_ -> a ; _ -> color blue $ rectangleSolid sx sy 
            ps = translate x y p 
        in pictures [ps,pb] where 

instance C.Handle Ship where
    --handle :: Event -> Ship -> IO Ship
    handle e s@Ship{controls = c} = c e s

instance C.Tick Ship where
    --tick :: Float -> Ship -> IO Ship
    tick f s@Ship{pos = p, speed = v, direction = d, gun = g, bullets =  bs, borders = bo, anim = a} = let 
        np = C.reBorder bo (p L.+ v L.* d)
        tb = C.tick f bs
        tg = C.tick f g
        (ng', nb) = shoot np (1,0) tg
        a' = case a of _:ps -> ps ; _ -> []
        in s {bullets = nb ++ tb, pos = np, gun = ng', anim = a' }

instance C.Collidable Ship where
    --size :: Ship ->  (Float,Float)
    size = size
    --position :: Ship -> (Float,Float)
    position = pos 
    repos v s@Ship{ pos = p } = s{ pos = p L.+ v } 
    reposChildren v s@Ship{ bullets = b } = s{ bullets = map (C.reposWithChildren v) b }