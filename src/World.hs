{-# LANGUAGE TypeApplications#-}
module World(World, startWorld, resetWorld, scroll) where

import Classess
import Graphics.Gloss
import qualified Ship as S(Ship, ship, bullets)
import qualified Enemy as E (Enemy, enemy, bullets, damage, deadly)
import Weapon(Bullet, PowerUp, dmg)
import System.Random
import Control.Arrow(second)

data World = World {
    player :: S.Ship, 
    enemies :: [E.Enemy],
    lives :: Int, 
    score :: Int,
    level :: Int,
    timer :: Float,
    powerUps :: [PowerUp],
    rng :: StdGen
}

startWorld :: StdGen -> World
startWorld seed = World {
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
        p <- tick f $ player w
        (rng', ne, np) <- damageAllEnemies (rng w) (S.bullets p) <$> tick f (enemies w)
        let t = timer (w::World) + f
        let nw =if  any (\e -> E.deadly e && checkCollision p e) ne || any (checkCollision p) (ne >>= E.bullets)
                then (resetWorld w) {lives = lives w -1} 
                else w {
                    player = p,
                    enemies = ne,
                    timer = t,
                    powerUps = np ++ powerUps w,
                    rng = rng'
                }
        return nw
        
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