module World where

import Classess
import Graphics.Gloss
import Ship as S(Ship, ship, bullets)
import Enemy as E(Enemy, enemy, bullets)
import Weapon(Bullet)

data World = World 
  {
  player :: Ship, 
  enemies :: [Enemy],
  lives :: Int, 
  score :: Int,
  level :: Int,
  timer :: Float
  }

world = World {
    player = ship,
    enemies = [enemy],
    lives = 3,
    score = 0,
    level = 0,
    timer = 0
} 

scroll :: Float -> World -> World
scroll xdelta w@World {player = p, enemies = e} = 
    w{ 
        enemies = map (reposWithChildren (xdelta, 0)) e 
    }

instance Paint World where
    paint w = do 
        pw <- paint $ player w
        pe <- paint $ enemies w
        let pt = translate 0 400 . color white . text . show . floor $ timer (w::World)  
        return $ pictures [pw, pt, pe]

instance Handle World where
    handle e w = do 
        p <- handle e $ player w
        return w {player = p}

instance Tick World where 
    tick f w = do 
        p <- tick f $ player w
        e <- tick f $ enemies w
        let t = timer (w::World)
        if checkHit p e 
            then pure world {lives = lives w -1} 
            else pure w {player = if checkHit p e 
                                    then ship 
                                    else p,
                                    enemies = filter (not . (checkHitCollidableBullet (S.bullets p))) e, 
                                    timer = t + f}
        where 
              checkHit :: Ship -> [Enemy] -> Bool
              checkHit p e  = checkHitPlayerEnemy p e  || checkHitCollidableBullet (concatMap E.bullets e) p 
              checkHitPlayerEnemy p e = or (map (checkCollision p) e) 
              checkHitCollidableBullet :: Collidable a => [Bullet] -> a -> Bool
              checkHitCollidableBullet b c = or (map (checkCollision c) b)