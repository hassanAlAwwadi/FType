{-# LANGUAGE MultiWayIf #-}
module Enemy where

import Resources(explosion)
import qualified Classess as C 
import Weapon(Gun, Bullet, PowerUp, simple, shoot, randomPowerUp)
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L
import System.Random


--import Weapon as W

data Enemy = Enemy{ size :: Float, pos :: Point, speed :: Float, direction :: Vector, health :: Float,   gun :: Gun, bullets :: [Bullet], deathAnim:: [Picture] } 
           | GraveMarker { pos :: Point, bullets :: [Bullet], deathAnim::[Picture] }
           deriving(Show)

instance C.Creatable Enemy where
    create stat _ = Enemy{ 
        Enemy.size = 20, 
        pos = (10,10), 
        speed = 5, 
        direction = (0,0), 
        health = 1, 
        gun = simple, 
        bullets = [],
        deathAnim = blank : explosion stat
    } 

damage :: Enemy -> Float -> StdGen -> (StdGen, (Enemy, Maybe PowerUp))
damage e@GraveMarker{} _ rng = (rng, (e, Nothing)) 
damage e@Enemy{health = h, pos = p} amount rng 
    | h > amount = (rng, (e{health = h - amount}, Nothing))
    | otherwise  = 
        let (randomVal, nextRng) = randomR (0::Int, 100) rng
            powerup = randomPowerUp randomVal
        in (nextRng, (GraveMarker{pos = pos e, bullets = bullets e, deathAnim = deathAnim e}, powerup p))

deadly :: Enemy -> Bool
deadly Enemy{} = True
deadly _ = False

instance C.Paint Enemy where
    paint Enemy{ size = s, pos = (x,y), bullets = b } = let
         pb = C.paint b
         pe = translate x y enemyDrawing 
         in pictures [pe,pb] where 
         enemyDrawing = color red $ rectangleSolid s s 
    paint GraveMarker{bullets = b, pos = (x,y), deathAnim = p:_} = pictures [translate x y $ scale 0.5 0.5 p, C.paint b]
    paint GraveMarker{bullets = b} = C.paint b

instance C.Tick Enemy where
    --tick :: Float -> Enemy -> IO Enemy
    tick f e@Enemy{ pos = p, speed = v, direction = d, gun = g, bullets = b} = let
        tb = C.tick f b
        tg = C.tick f g
        (sg, nb) = shoot  p d  tg
        in e { pos = p L.+ v L.* d, bullets = nb++tb, gun = sg }
    
    -- | gravemarker updates the bullets bullets and plays the animation
    tick f g@GraveMarker{bullets = b, deathAnim = _:ps } = g{bullets = C.tick f b, deathAnim=ps}
    tick f g@GraveMarker{bullets = b} = 
        g{bullets = C.tick f b}
    
instance C.Collidable Enemy where
    --size :: Enemy ->  (Float,Float)
    size GraveMarker{} = (0,0)
    size Enemy{size = s} = (s, s)
    --position :: Enemy -> (Float,Float)
    position = pos
    repos v e = e{pos = pos e L.+ v} 
    reposChildren v e = e{bullets = C.reposWithChildren v <$> bullets e}
