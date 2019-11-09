{-# LANGUAGE DuplicateRecordFields, InstanceSigs #-}
module Lib
    ( someFunc
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game(playIO)

import Classess
import Game as G
import Resources

someFunc :: IO ()
someFunc = do
    -- get seed
    seed <- getSeed
    -- get explosion animation
    explosionBMPs <- getExplosions
    -- initial game 
    let initialGame = create (StaticResource explosionBMPs) (DynamicResource seed) :: Game
    -- "smart" constructor of menu is used to create the main menu
    playIO FullScreen black 30 initialGame paintIO handleIO tickIO