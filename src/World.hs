{-# LANGUAGE TypeApplications#-}
module World(World, startWorld, resetWorld, scroll) where


import Data.List(partition)
import System.Random
import Control.Arrow(second)

import Classess
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Ship as S(Ship, ship, bullets, powerUp,pos)
import qualified Enemy as E (Enemy,Enemy(GraveMarker), enemy, bullets, damage, deadly,direction,pos)
import Weapon(Bullet, PowerUp, dmg)

data World = World {
    player :: S.Ship, 
    enemies :: [E.Enemy],
    lives :: Int, 
    score :: Int,
    level :: Int,
    timer :: Float,
    powerUps :: [PowerUp],
    totalScore :: Int,
    rng :: StdGen
}

startWorld :: StdGen -> World
startWorld seed = World {
    totalScore = 0,
    player = S.ship,
    enemies = [E.enemy],
    lives = 3,
    score = 0,
    level = 0,
    timer = 0,
    powerUps = [],
    rng = seed
} 

resetWorld :: World -> World
resetWorld world = world {
    player = S.ship,
    enemies = [E.enemy],
    lives = 3,
    score = 0,
    level = 0,
    timer = 0
}

scroll :: Float -> World -> World
scroll xdelta w@World { enemies = e } = w{ enemies = map (reposWithChildren (xdelta, 0)) e }

instance Paint World where
    paint World{player = p, enemies = es, powerUps = pus, timer = t} = let 
        pw = paint p
        pe = paint es
        pu = paint pus
        --type app used to prevent defaulting of show
        pt = translate 0 400 . color white . text . show @ Integer $ floor t 
        in pictures [pw, pt, pu, pe]


instance Handle World where
    -- just pass the handle event to the player ship
    handle e w =  w {player = handle e $ player w}

instance Tick World where 
    tick f w = let 
        -- simple ticks
        p = tick f $ player w
        e = tick f $ enemies w
        t = timer (w::World) + f

        -- damage enemies after the tick event
        (nextRNG, e', newups) = damageAllEnemies (rng w) (S.bullets p) e
        ups = newups ++ powerUps w

        -- upgrade the player with powerups
        (touchedUps, freeUps) = partition (checkCollision p) ups
        upgradedP = foldr (flip S.powerUp) p touchedUps

        e'' | (round t) `mod` 2 == 0 = spawnEnemy e'
            | otherwise = e'
        -- let enemies move to player
        eToP =  replaceDirection e'' upgradedP
        -- reset world if player is hit
        -- update it otherwise
        nw = if  playerhit p e
             then (resetWorld w) {lives = lives w -1, totalScore = totalScore w + floor t} 
             else w {
                player = upgradedP,
                enemies = eToP,
                timer = t,
                powerUps = freeUps,
                rng = nextRNG
                }
                 
        in nw where
            playerhit p es = any (\e -> E.deadly e && checkCollision p e) es || any (checkCollision p) (es >>= E.bullets)
        
damageAllEnemies :: StdGen -> [Bullet] -> [E.Enemy] -> (StdGen, [E.Enemy], [PowerUp])
damageAllEnemies seed bs es = 
    let 
        enemyHitBy :: [(E.Enemy,[Bullet])]
        enemyHitBy  = map (\e -> (e, filter (checkCollision e) bs)) es
        enemyDamage :: [(E.Enemy, Float)]
        enemyDamage = map (second sum . second (map dmg)) enemyHitBy
        go :: (E.Enemy, Float) -> (StdGen, [E.Enemy], [PowerUp]) -> (StdGen, [E.Enemy], [PowerUp])
        go (e, float) (gen, eacc, pacc) = let (gen', (en, pu)) = E.damage e float gen in (gen', en:eacc, maybe pacc (:pacc) pu)
    in  foldr go (seed, [], []) enemyDamage 

replaceDirection :: [E.Enemy] -> S.Ship -> [E.Enemy]
replaceDirection e s = map shipDirection e
                    where shipDirection e'@(E.GraveMarker _ _) =e'
                          shipDirection e' = e' {E.direction = forward (normalizeV (calcVector (E.pos e') (S.pos s))) }
                          calcVector :: Vector -> Vector -> Vector
                          calcVector (x1,y1) (x2,y2) =(x2-x1, y2-y1)
                          forward (x,y) = (- (abs x),y)

spawnEnemy :: [E.Enemy] -> [E.Enemy]
spawnEnemy e = e3:e2:newEnemy:e
            where newEnemy = E.enemy {E.pos = (1000,10) }
                  e2 = E.enemy {E.pos = (1000,-500) }
                  e3 = E.enemy {E.pos = (1000,500) }