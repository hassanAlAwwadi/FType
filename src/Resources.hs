module Resources where

import Graphics.Gloss
import System.Random(StdGen, newStdGen)

-- | resources that will stay the same throughout the games lifespan
data StaticResource = StaticResource   { explosion :: [Picture] }

getExplosions :: Int -> IO [Picture]
getExplosions lifespan = mapM loadBMP ["explosion/explosion_" ++ show n ++ ".bmp" |  n <- [1..12]::[Int], _ <- [1.. max 1 $ lifespan `div` 12] ]

-- | resources that will change throughout the games lifespan
-- | Make sure to update your copy, etc...
data DynamicResource = DynamicResource { rng :: StdGen}

getSeed :: IO StdGen
getSeed = newStdGen