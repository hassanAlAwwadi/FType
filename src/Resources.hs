module Resources where

import Control.Arrow((***))
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import System.Random(StdGen, newStdGen)

-- | resources that will stay the same throughout the games lifespan
data StaticResource = StaticResource   { explosion :: [Picture], border::Border }

getExplosions :: Int -> IO [Picture]
getExplosions lifespan = mapM loadBMP ["explosion/explosion_" ++ show n ++ ".bmp" |  n <- [1..12]::[Int], _ <- [1.. max 1 $ lifespan `div` 12] ]

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