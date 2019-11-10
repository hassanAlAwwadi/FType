module Resources where

import Control.Arrow((***))
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import System.Random(StdGen, newStdGen)

-- | resources that will stay the same throughout the games lifespan
data StaticResource = StaticResource   { explosion, player1Ship, player2Ship :: [Picture], border::Border }

getStaticResource :: IO StaticResource
getStaticResource = do
    ex <- getExplosions
    p1i <- getPlayer1Ship
    p2i <- getPlayer2Ship
    bo <- getBorders
    pure $ StaticResource { explosion = ex, player1Ship = p1i, player2Ship = p2i, border = bo}

getExplosions :: IO [Picture]
getExplosions = mapM loadBMP ["explosion/explosion_" ++ show n ++ ".bmp" |  n <- [1..12::Int]]

getPlayer1Ship :: IO [Picture]
getPlayer1Ship = mapM loadBMP ["ship/p1_" ++ show n ++ ".bmp" | n <- [1..4::Int]]

getPlayer2Ship :: IO [Picture]
getPlayer2Ship = mapM loadBMP ["ship/p2_" ++ show n ++ ".bmp" | n <- [1..4::Int]]

data Border = Border { xmax, ymax, xmin, ymin::Float } deriving Show

getBorders :: IO Border
getBorders = do
    (xs,ys) <- fmap (fromIntegral *** fromIntegral) getScreenSize 
    return Border{ xmax = xs/2, xmin = - xs/2, ymax =   ys/2, ymin = - ys/2 }


-- | resources that will change throughout the games lifespan
-- | Make sure to update your copy, etc...
data DynamicResource = DynamicResource { rng :: StdGen}

getSeed :: IO StdGen
getSeed = newStdGen