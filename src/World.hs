module World where

import Classess
import Graphics.Gloss
import Ship(Ship,ship)
import Enemy(Enemy,bullets)

data World = World 
  {
  player :: Ship, 
  enemies :: [Enemy],
  lives :: Int, 
  score :: Int,
  level :: Int,
  timer :: Float
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
        pure w {player = if checkHit p e then p else ship,
         enemies = e,
         lives =  if checkHit p e then (lives w - 1) else lives w,
         timer = t + f}
        where checkHit p e  = checkHitPlayerEnemy p e  -- || checkHitPlayerBullet p (concatMap enemyBullets e)
              checkHitPlayerEnemy p e = or (map (checkCollision p) e) 
              checkHitPlayerBullet p b = or (map (checkCollision p) b)
              enemyBullets e = bullets e
