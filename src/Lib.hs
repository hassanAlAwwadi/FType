{-# LANGUAGE DuplicateRecordFields, InstanceSigs #-}
module Lib
    ( playGame
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game(playIO)

import Classess
import Game as G
import Resources

playGame :: IO ()
playGame = do
    -- get static resource
    st <- getStaticResource
    -- get seed
    seed <- getSeed
    -- initial game 
    let initialGame = create st (DynamicResource seed) :: Game
    -- "smart" constructor of menu is used to create the main menu
    playIO FullScreen black 30 initialGame paintA handleIO tickA

 