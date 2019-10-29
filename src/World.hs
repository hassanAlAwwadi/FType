module World where

import Classess
import Graphics.Gloss
import Ship(Ship, ship)
import Enemy(Enemy, enemy)
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
        pure w {player = p, enemies = e, timer = t + f}

