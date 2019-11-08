{-# LANGUAGE MultiWayIf #-}
module Enemy where

import qualified Classess as C 
import Weapon(Gun, Bullet, PowerUp, simple, shoot, randomPowerUp)
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as L
import System.Random


--import Weapon as W

data Enemy = Enemy{ size :: Float, pos :: Point, speed :: Float, direction :: Vector, health :: Float,   gun :: Gun, bullets :: [Bullet] } 
           | GraveMarker { pos :: Point, bullets :: [Bullet] }

enemy :: Enemy
enemy = Enemy{ 
    Enemy.size = 10, 
    pos = (10,10), 
    speed = 5, 
    direction = (0,0), 
    health = 10, 
    gun = simple, 
    bullets = []
}

damage :: Enemy -> Float -> StdGen -> (StdGen, (Enemy, Maybe PowerUp))
damage e@GraveMarker{} _ rng = (rng, (e, Nothing)) 
damage e@Enemy{health = h, pos = p} amount rng 
    | h > amount = (rng, (e{health = h - amount}, Nothing))
    | otherwise  = 
        let (randomVal, nextRng) = randomR (0::Int, 100) rng
            powerup = randomPowerUp randomVal
        in (nextRng, (GraveMarker{pos = pos e, bullets = bullets e}, powerup p))

deadly :: Enemy -> Bool
deadly Enemy{} = True
deadly _ = False

instance C.Paint Enemy where
    paint Enemy{ size = s, pos = (x,y), bullets = b } = do
         pb <- C.paint b
         let pe = translate x y enemyDrawing 
         pure $ pictures [pe,pb] where 
         enemyDrawing = color red $ rectangleSolid s s 
    paint GraveMarker{bullets = b} = C.paint b

instance C.Tick Enemy where
    --tick :: Float -> Enemy -> IO Enemy
    tick f e@Enemy{ pos = p, speed = v, direction = d, gun = g, bullets = b} = do
        tb <- C.tick f b
        tg <- C.tick f g
        let (sg, nb) = shoot  p d  tg
        pure e { pos = p L.+ v L.* d, bullets = nb++tb, gun = sg }
    tick f g@GraveMarker{bullets = b} = do
        nb <- C.tick f b
        return g{bullets = nb}
    
instance C.Collidable Enemy where
    --size :: Enemy ->  (Float,Float)
    size GraveMarker{} = (0,0)
    size Enemy{size = s} = (s, s)
    --position :: Enemy -> (Float,Float)
    position = pos
    repos v e = e{pos = pos e L.+ v} 
    reposChildren v e = e{bullets = C.reposWithChildren v <$> bullets e}
