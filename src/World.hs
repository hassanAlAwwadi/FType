{-# LANGUAGE TypeApplications#-}
module World(World, withPlayer2, lives, score, resetWorld, scroll, stat, dyn) where


import Data.List(partition)
import Control.Arrow(second)
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Classess
import Resources

import qualified Ship as S(Ship, bullets, powerUp,pos, player2)
import qualified Enemy as E (Enemy,Enemy(GraveMarker),gun,health, bullets, damage, deadly,direction,pos, cullTarget)
import Weapon(Bullet, PowerUp, dmg,spreadShot)

data World = World {
    players :: [S.Ship], 
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
        players = [create s d],
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

withPlayer2 :: World -> World 
withPlayer2 w@World{stat = s, dyn = d} = w{players = [create s d, repos (0,100) $ S.player2 (create s d)]}

resetWorld :: World -> World
resetWorld w@World{stat = s, dyn = d} = w{
    players = case players w of [_] -> [create s d] ; [_, _] -> [create s d, repos (0,100) $ S.player2 (create s d)] ; _ -> [],
    enemies = [],
    lives = 3,
    level = 0,
    timer = 0,
    powerUps = []
}

scroll :: Float -> World -> World
scroll xdelta w@World { enemies = e } = w{ enemies = map (reposWithChildren (xdelta, 0)) e }

instance Paint World where
    paint World{players = p, enemies = es, powerUps = pus, timer = t} = let 
        pw = paint p
        pe = paint es
        pu = paint pus
        --type app used to prevent defaulting of show
        pt = translate 0 400 . color white . text . show @ Integer $ floor t 
        in pictures [pw, pt, pu, pe]


instance Handle World where
    -- just pass the handle event to the player1 ship
    handle e w =  w {players = handle e <$> players w}

instance Tick World where 
    tick f world = let 
        -- scroll the entire world
        w = scroll f world
        -- simple ticks
        ps = tick f  <$> players w

        e = tick f $ enemies w
        t = timer (w::World) + f

        -- remove enemies that have gone out of the border range
        e' =  filter (not . E.cullTarget) e
        -- damage enemies after the tick event
        (nextRNG, e'', newups) = damageAllEnemies (rng $ dyn w) (ps >>= S.bullets) e'
        ups = newups ++ powerUps w

        -- upgrade the player with powerups
        (upgradedPs, remainingUps) = upgradeMulti ps ups []

        enext = if round t `mod` 5 == 0 && round (timer (w::World)) `mod` 5 /= 0 
               then spawnEnemy (stat w) (dyn w) e''
               else e''

        -- let enemies move to player1

        eToP =  replaceDirection enext upgradedPs
        -- reset world if player1 is hit
        -- update it otherwise
        nw = if  playerhit eToP upgradedPs 
             then (resetWorld w) {lives = lives w -1, score = score w + floor t} 
             else w {
                players = upgradedPs,
                enemies = eToP,
                timer = t,
                powerUps = remainingUps,
                dyn = (dyn w){ rng = nextRNG }
                }
                 
        in nw 

playerhit :: [E.Enemy] -> [S.Ship] -> Bool
playerhit es ps = or [checkCollision a b | a <- ps, b <- es, E.deadly b] || or [checkCollision a b | a <- ps, b <- es >>= E.bullets]

upgradeSingle :: S.Ship -> [PowerUp] -> S.Ship
upgradeSingle = foldr (flip S.powerUp)

checkTouch ::   S.Ship -> [PowerUp] -> ([PowerUp], [PowerUp])
checkTouch p = partition (checkCollision p)

upgradeMulti :: [S.Ship] -> [PowerUp] -> [S.Ship] -> ([S.Ship],[PowerUp])
upgradeMulti (p:ps) ups acc = let 
    (touched, remaining) = checkTouch p ups
    pUpp = upgradeSingle p touched
    nAcc = pUpp : acc
    in upgradeMulti ps remaining nAcc
upgradeMulti [] ups acc = (acc, ups)
                        
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

--redirects the enemies to the player
replaceDirection :: [E.Enemy] -> [S.Ship] -> [E.Enemy]
replaceDirection e s = map replaceDirection' (zip e (map lowestDistShip e))
                            --find the closest ship for all enemies
                        where lowestDistShip e' = snd (minimum (zip (map (distance (E.pos e'))  (map S.pos s)) (map S.pos s)))
                              distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
                               where x' = x1 - x2
                                     y' = y1 - y2

replaceDirection' :: (E.Enemy, Point) -> E.Enemy
replaceDirection' (e,sp) = shipDirection e
                    where shipDirection e'@E.GraveMarker{} = e'
                          shipDirection e' = e' {E.direction = maxAngle (normalizeV (calcVector (E.pos e') sp)) }
                          calcVector :: Vector -> Vector -> Vector
                          calcVector (x1,y1) (x2,y2) =(x2-x1, y2-y1)
                          --don't let ships move backward or upward, only forward with a max upward angle of 80%
                          maxAngle d@(x,y) | x>0           =  (-1,y)
                                           | y> maxAngle'  =  (-1,maxAngle')
                                           | y< -maxAngle' =  (-1,-maxAngle')
                                           | otherwise     = d
                                        where maxAngle' = 0.8
--spawn 5 enemys with 2 spreadshot enemies with extra life
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
        