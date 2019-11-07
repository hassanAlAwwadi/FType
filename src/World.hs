{-# LANGUAGE TypeApplications#-}
module World(World, startWorld, resetWorld, scroll) where


import Data.List(partition)
import System.Random
import Control.Arrow(second)

import Classess
import Graphics.Gloss
import qualified Ship as S(Ship, ship, bullets, powerUp)
import qualified Enemy as E (Enemy, enemy, bullets, damage, deadly)
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
    paint World{player = p, enemies = es, powerUps = pus, timer = t} = do 
        pw <- paint p
        pe <- paint es
        pu <- paint pus
        --type app used to prevent defaulting of show
        let pt = translate 0 400 . color white . text . show @ Integer $ floor t 
        return $ pictures [pw, pt, pu, pe]


instance Handle World where
    handle e w = do 
        p <- handle e $ player w
        return w {player = p}

instance Tick World where 
    tick f w = do 
        -- simple ticks
        p <- tick f $ player w
        e <- tick f $ enemies w
        let t = timer (w::World) + f

        -- damage enemies after the tick event
        let (nextRNG, e', newups) = damageAllEnemies (rng w) (S.bullets p) e
        let ups = newups ++ powerUps w

        -- upgrade the player with powerups
        let (touchedUps, freeUps) = partition (checkCollision p) ups
        let upgradedP = foldr (flip S.powerUp) p touchedUps

        -- reset world if player is hit
        -- update it otherwise
        let nw = if  playerhit p e
                 then (resetWorld w) {lives = lives w -1, totalScore = totalScore w + floor t} 
                 else w {
                    player = upgradedP,
                    enemies = e',
                    timer = t,
                    powerUps = freeUps,
                    rng = nextRNG
                 }
                 
        return nw where
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