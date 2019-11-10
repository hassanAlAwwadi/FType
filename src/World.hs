{-# LANGUAGE TypeApplications#-}
module World(World, lives, score, resetWorld, scroll, stat, dyn) where


import Data.List(partition)
import System.Random
import Control.Arrow(second)

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Classess
import Resources

import qualified Ship as S(Ship, bullets, powerUp,pos)
import qualified Enemy as E (Enemy,Enemy(GraveMarker),gun,health, bullets, damage, deadly,direction,pos, cullTarget)
import Weapon(Bullet, PowerUp, dmg,spreadShot)

data World = World {
    player :: S.Ship, 
    enemies :: [E.Enemy],
    lives :: Int, 
    score :: Int,
    level :: Int,
    timer :: Float,
    powerUps :: [PowerUp],
    state :: WorldState,
    stat :: StaticResource,
    dyn :: DynamicResource
}

instance Creatable World where
    create s d = World {
        player = create s d,
        enemies = [],
        lives = 3,
        score = 0,
        level = 0,
        timer = 0,
        powerUps = [],
        stat = s,
        dyn = d,
        state = Scrolling 0.5
    }
    
resetWorld :: World -> World
resetWorld w@World{stat = s, dyn = d} = w{
    player = create s d,
    enemies = [],
    lives = 3,
    level = 0,
    timer = 0,
    powerUps = []
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
    tick f world = let 
        -- scroll the entire world
        w = scroll f world
        -- simple ticks
        p = tick f $ player w
        e = tick f $ enemies w
        t = timer (w::World) + f

        -- remove enemies that have gone out of the border range
        e' =  filter (not . E.cullTarget) e
        -- damage enemies after the tick event
        (nextRNG, e'', newups) = damageAllEnemies (rng $ dyn w) (S.bullets p) e'
        ups = newups ++ powerUps w

        -- upgrade the player with powerups
        (touchedUps, freeUps) = partition (checkCollision p) ups
        upgradedP = foldr (flip S.powerUp) p touchedUps

        e''' | round t `mod` 5 == 0 && round (timer (w::World)) `mod` 5 /= 0 = spawnEnemy (stat w) (dyn w) e''
             | otherwise = e''

        -- let enemies move to player
        eToP =  replaceDirection e''' upgradedP
        -- reset world if player is hit
        -- update it otherwise
        nw = if  playerhit p e
             then (resetWorld w) {lives = lives w -1, score = score w + floor t} 
             else w {
                player = upgradedP,
                enemies = eToP,
                timer = t,
                powerUps = freeUps,
                dyn = (dyn w){ rng = nextRNG }
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
                    where shipDirection e'@E.GraveMarker{} = e'
                          shipDirection e' = e' {E.direction = maxAngle (normalizeV (calcVector (E.pos e') (S.pos s))) }
                          calcVector :: Vector -> Vector -> Vector
                          calcVector (x1,y1) (x2,y2) =(x2-x1, y2-y1)
                          maxAngle d@(x,y) | x>0           =  (-1,y)
                                           | y> maxAngle'  =  (-1,maxAngle')
                                           | y< -maxAngle' =  (-1,-maxAngle')
                                           | otherwise     = d
                                        where maxAngle' = 0.8

spawnEnemy :: StaticResource -> DynamicResource -> [E.Enemy] -> [E.Enemy]
spawnEnemy s d e = e5:e4:e3:e2:e1:e
            where template = create s d
                  e1 = template {E.pos = (1000,0) }
                  e2 = template {E.pos = (1000,-400) ,E.gun = spreadShot, E.health = 3 }
                  e3 = template {E.pos = (1500,400) ,E.gun = spreadShot, E.health = 3}
                  e4 = template {E.pos = (1500,700) }
                  e5 = template {E.pos = (1000,-700) }

data WorldState = Paused    { past :: WorldState } 
                | Scrolling { scrollSpeed :: Float } 
             -- | for now there is still no way to get in or out of a bossfight
                | BossFight { boss :: E.Enemy } deriving (Show)
        

