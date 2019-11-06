{-# LANGUAGE MultiWayIf #-}
module Enemy where

import qualified Classess as C 
import Weapon(Gun, Bullet, simple, shoot, PowerUp(..))
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L
import System.Random


--import Weapon as W

data Enemy = Enemy{ size :: Float, pos :: Point, speed :: Float, direction :: Vector, health :: Float,   gun :: Gun, bullets :: [Bullet] } 
           | GraveMarker { pos :: Point, reward :: Maybe PowerUp, bullets :: [Bullet] }

enemy = Enemy{ 
    Enemy.size = 10, 
    pos = (10,10), 
    speed = 5, 
    direction = (0,0), 
    health = 10, 
    gun = simple, 
    bullets = []
    }

damage :: Enemy -> Float -> StdGen -> (StdGen, Enemy)
damage e@GraveMarker{} _ rng = (rng, e) 
damage e@Enemy{health = h} amount rng 
    | h > amount = (rng, e{health = h - amount})
    | otherwise  = 
        let (randomVal, nextRng) = randomR (0::Int, 100) rng
            powerup =   if | randomVal <= 5  -> Just PUp
                           | randomVal <= 10 -> Just SUp
                           | randomVal <= 15 -> Just LUp
                           | otherwise       -> Nothing
        in (nextRng, GraveMarker{pos = pos e, reward = powerup, bullets = bullets e})



instance C.Paint Enemy where
    paint Enemy{ size = s, pos = (x,y), bullets = b } = do
         pb <- C.paint b
         let pe = translate x y enemyDrawing 
         pure $ pictures [pe,pb] where 
         enemyDrawing = color red $ rectangleSolid s s 

instance C.Tick Enemy where
    --tick :: Float -> Enemy -> IO Enemy
    tick f e@Enemy{ pos = p, speed = v, direction = d, gun = g, bullets = b} = do
        tb <- C.tick f b
        tg <- C.tick f g
        let (sg, nb) = shoot  p (-1, 0)  tg
        pure e { pos = p L.+ v L.* d, bullets = nb++tb, gun = sg }
    
instance C.Collidable Enemy where
    --size :: Enemy ->  (Float,Float)
    size e = (Enemy.size e, Enemy.size e)
    --position :: Enemy -> (Float,Float)
    position = pos
    repos v e@Enemy{pos = p} = e{pos = p L.+ v} 
    reposChildren v e@Enemy{bullets = b} = e{bullets = map (C.reposWithChildren v) b}
