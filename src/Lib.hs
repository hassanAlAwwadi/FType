{-# LANGUAGE DuplicateRecordFields, InstanceSigs #-}
module Lib
    ( someFunc
    ) where

import System.IO (readFile)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import qualified Data.Ord as D (Down(..), comparing) 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import Classess as C
import World
import Menu
import System.Random

someFunc :: IO ()
someFunc = do
    -- get seed
    seed <- newStdGen
    -- get explosion animation
    explosionBMPs <- getExplosions
    
    -- "smart" constructor of menu is used to create the main menu
    let mainMenu = createMenu [("play game", playGame seed), ("HighScore", loadHighScore)]
    playIO FullScreen black 30 MainMenu{ menu = mainMenu} paintIO handleIO tickIO

getExplosions :: IO [Picture]
getExplosions = mapM loadBMP []

data WorldState = Paused    { past ::WorldState } 
                | Scrolling {scrollSpeed :: Float} 
                -- | for now there is still no way to get in or out of a bossfight
                | BossFight {} deriving (Show)

pause :: WorldState -> WorldState
pause (Paused s) = s
pause  s         = Paused s

data GameState = Playing    { world :: World, state :: WorldState} 
               | MainMenu   { menu  :: Menu GameState} 
               | HighScores { mvps  :: IO [(String, Float)]}


--level 1 is wip, ok? OK?
playGame :: StdGen -> GameState
playGame seed = Playing{ world = startWorld seed, state = Scrolling (-0.5)}

--loading the highscores
loadHighScore :: GameState
loadHighScore = 
    let scores = do 
            file <- readOrCreateFile  "HighScores.txt" 
            let filelines = lines file
            pure $ sortBy (D.comparing $ D.Down . snd) $ map read filelines
    in HighScores { mvps = scores }

--crashing the game is ugly so this makes sure that the game survives, even if the highscore file wasn't created
readOrCreateFile :: FilePath -> IO String
readOrCreateFile p = do 
    appendFile p ""
    readFile p 

instance PaintIO GameState where
    --paint menu
    paintIO MainMenu{ menu = m }         = pure $ paint m
    --paint highscores
    paintIO HighScores{ mvps = ioNames } = do
        scores <- ioNames
        let nameandscores = map (\(name, score) -> Text $ name ++ ":" ++ show score) scores
        let translated = uncurry (translate 0) <$> zip [0, (-200)..] nameandscores
        let single = color white $ pictures translated
        return $ translate (-900) 300 single
    --paint paused game
    paintIO Playing{ world = w, state = Paused s } = do 
        let pw = paint w
        let pauseScreen = translate (-900) (-100) $ scale 4 4 $ color white $ text $ show (Paused s)
        pure $ pictures [pw, pauseScreen]
    --paint game
    paintIO Playing{ world = w } = pure $ paint w



instance HandleIO GameState where 
    -- the special keys. Exit game, pause game, menu action
    handleIO (EventKey (SpecialKey KeyEsc)   Down _ _) _               = exitSuccess 
    handleIO (EventKey (Char 'p') Down _ _) g@Playing{ state = s } = pure $ g{ state = pause s }
    -- if for some the mainmenu failst to activate, it will just stay in the menu
    handleIO (EventKey (SpecialKey KeyEnter) Down _ _) mm@MainMenu{ menu = m }  = pure $ fromMaybe mm $ menuAction m 
    -- moving the menu
    handleIO e mm@MainMenu{ menu = m }  = return $ mm{ menu = handle e m}
    -- playing the game
    handleIO e g@Playing{ world = w } = return $ g{ world = handle e w }
    -- Bossfight WIP
    handleIO _ p = pure p

instance TickIO GameState where
    -- tick doesn't do anything if the game is paused
    tickIO _ g@Playing{state = Paused _} = pure g
    -- No scrolling screen during bossfight
    tickIO f g@Playing{ world = w, state = BossFight} = return $ g{ world = tick f w }
    -- yes scrolling screen during non boss fights
    tickIO f g@Playing{ world = w, state = Scrolling h } = return $ g{ world = tick f $ scroll h w }
    -- menu/highscore tick doesn't do anything
    tickIO _ g = pure g  


