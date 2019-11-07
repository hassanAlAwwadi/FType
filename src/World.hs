module World(World, startWorld, resetWorld, scroll) where

import Classess
import Graphics.Gloss
import Ship as S(Ship, ship, bullets)
import Enemy as E(Enemy, enemy, bullets)
import Weapon(Bullet)
import System.Random

data World = World {
    player :: Ship, 
    enemies :: [Enemy],
    lives :: Int, 
    score :: Int,
    level :: Int,
    timer :: Float,
    rng :: StdGen
}

startWorld :: StdGen -> World
startWorld seed = World {
    player = ship,
    enemies = [enemy],
    lives = 3,
    score = 0,
    level = 0,
    timer = 0,
    rng = seed
} 

resetWorld :: World -> World
resetWorld world = world {
    player = ship,
    enemies = [enemy],
    lives = 3,
    score = 0,
    level = 0,
    timer = 0
}

scroll :: Float -> World -> World
scroll xdelta w@World { enemies = e } = w{ enemies = map (reposWithChildren (xdelta, 0)) e }

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
        let t = timer (w::World) + f
        if checkHit p e 
            then pure (resetWorld w) {lives = lives w -1} 
            else pure w {player = if checkHit p e 
                                    then ship 
                                    else p,
                                    enemies = filter (not . checkHitCollidableBullet (S.bullets p)) e, 
                                    timer = t }
        where 
              checkHit :: Ship -> [Enemy] -> Bool
              checkHit p e  = any (checkCollision p) e   || any (checkCollision p) (e >>= E.bullets) 
              checkHitCollidableBullet :: Collidable a => [Bullet] -> a -> Bool
              checkHitCollidableBullet b c = any (checkCollision c) b