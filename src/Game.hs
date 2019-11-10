module Game where

import System.Exit(exitSuccess)
import System.IO (readFile) 
import Data.Maybe as M(fromMaybe)
import Data.List as S(sortBy) 
import Data.Char
import qualified Data.Ord as D (Down(..), comparing) 

import Classess as C
import Resources as R(StaticResource, DynamicResource)
import qualified World as W(stat, dyn, score)
import World(World, lives)
import Menu(Menu, createMenu, menuAction)

import Graphics.Gloss.Interface.IO.Game

data Game   = Playing      { world :: World} 
            | Menu         { menu  :: Menu (IO Game) } 
            -- | Highcores are really the only reason we need to use playIO instead of regular play
            | HighScores   { mvps  :: [(String, Float)], scrollUp :: Bool, scrollDown :: Bool, scrollDelta :: Float, stat :: StaticResource, dyn :: DynamicResource }
            | NewHighScore { nameWIP :: String, score :: Int, stat :: StaticResource, dyn :: DynamicResource }


instance Creatable Game where
    create s d = Menu $ mainMenu s d

mainMenu :: StaticResource -> DynamicResource -> Menu (IO Game)
mainMenu s d = createMenu [
    ("play game", pure $ Playing w), 
    ("HighScore", loadHighScore s d "HighScores.txt" ),
    ("Quit Game", exitSuccess)
    ] where
    w = create s d

pauseMenu :: World -> Menu (IO Game)
pauseMenu  w = createMenu [
    ("Resume game", pure $ Playing w), 
    ("Main menu", pure $ Menu $ mainMenu (W.stat w) (W.dyn w) ),
    ("Quit Game", exitSuccess)
    ]

--loading the highscores
loadHighScore :: StaticResource -> DynamicResource -> String -> IO Game
loadHighScore st dn fileName = do 
    file' <- readOrCreateFile fileName
    let filelines = lines file'
    let scores = sortBy (D.comparing $ D.Down . snd) $ map read filelines
    return $ HighScores { mvps = scores, scrollDown = False, scrollUp = False, scrollDelta = 0, stat = st, dyn = dn }

--write the highscores
writeHighScore :: Int -> IO String
writeHighScore s = do 
            file <- readOrCreateFile  "HighScores.txt" 
            length file `seq` writeFile "HighScores.txt" ("(\"Player1\", "++ show s ++")\n")
            appendFile "HighScores.txt" file
            return file
        
--crashing the game is ugly so this makes sure that the game survives, even if the highscore file wasn't created
readOrCreateFile :: FilePath -> IO String
readOrCreateFile p = do 
    appendFile p ""
    readFile p 

instance Paint Game where
    --paint menu
    paint Menu{ menu = m }           = paint m
    --paint highscores
    paint HighScores{ mvps = names, scrollDelta = sd} = let
        nameandscores = map (\(name, s) -> scale 0.6 0.6 $ Text $ name ++ ":" ++ show s) names
        translated = uncurry (translate 0) <$> zip [0, (-200)..] nameandscores
        single = color white $ pictures translated
        in translate (-900) (sd + 400) single
    --paint game
    paint Playing{ world = w } = paint w
    paint NewHighScore{ nameWIP = n, score = s} = pictures $ map (color white) [
        translate (-700) 0      $ text $ "Your Name: \n" ++ n ++ "_", 
        translate (-700) (-250) $ text $ "Your Score: " ++ show s
        ]



instance HandleIO Game where 
    -- the special keys. Exit game, pause game, menu action
    handleIO (EventKey (SpecialKey KeyEsc)   Down _ _) _               = exitSuccess 
    handleIO (EventKey (Char 'p') Down _ _) Playing{world = w} = pure $ Menu $ pauseMenu w
    -- if for some the mainmenu failst to activate, it will just stay in the menu
    handleIO (EventKey (SpecialKey KeyEnter) Down _ _) mm@Menu{ menu = m }  = fromMaybe (pure mm) $ menuAction m 
    -- moving the menu
    handleIO e mm@Menu{ menu = m }  = return $ mm{ menu = handle e m}
    -- playing the game
    handleIO e g@Playing{ world = w } = return $ g{ world = handle e w }
    
    --WriteHighScore 
    handleIO e g@NewHighScore{ nameWIP = n, score = s, stat = st, dyn = dn} = case e of 
        EventKey (Char '\b') Down _ _ -> return $ g{ nameWIP = take (length n -1) n}
        EventKey (Char c) Down _ _ -> return $ if isAlphaNum c then g{ nameWIP = n ++ [c]} else g
        EventKey (SpecialKey KeySpace) Down _ _ -> return $ g{nameWIP = n ++ " "}
        -- WIP: EventKey (SpecialKey KeyBackspace) Down _ _ -> return $ g{nameWIP = n ++ " "}
        EventKey (SpecialKey KeyEnter) Down _ _ -> do
            appendFile "HighScores.txt" $ '\n' : show (n, s)
            loadHighScore st dn "HighScores.txt"
        _ -> return g
    -- you can scroll up and down for highscores
    handleIO e g@HighScores{stat = st, dyn = dn} =pure $ case e of
        EventKey (Char 'w' ) Down _ _ -> g{scrollUp   = True}
        EventKey (Char 's' ) Down _ _ -> g{scrollDown = True}
        EventKey (Char 'w' ) Up   _ _ -> g{scrollUp   = False}
        EventKey (Char 's' ) Up   _ _ -> g{scrollDown = False}
        EventKey (Char '\b') Down _ _ -> Menu $ mainMenu st dn
        _                             -> g

instance Tick Game where
    tick f g@Playing{ world = w } | lives w == 0 = NewHighScore {nameWIP = "", Game.score = W.score w, stat = W.stat w, dyn = W.dyn w}
                                  | otherwise  =  g{ world = tick f w }
    -- menu/highscore tick doesn't do anything
    tick _ g@HighScores{scrollUp = up, scrollDown = down, scrollDelta = d} 
        | up   = g{scrollDelta = d + 5}
        | down = g{scrollDelta = d - 5}
        | otherwise  = g
    tick _ g = g  
