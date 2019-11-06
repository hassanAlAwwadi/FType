module Menu(Menu, createMenu, menuAction) where

import Classess
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Menu a = Menu { menuItems :: [(String, a -> a)], maxSelected :: Int, selected :: Int, emptylist :: Bool }

instance Paint (Menu a) where
    paint Menu{emptylist = True} = pure Blank
    paint Menu {selected = s, menuItems = items} = 
        let menuSelector = translate 400 (fromIntegral $ 50 + s * (-250)) $ color yellow $ rectangleWire 900 200
            labels = translate 0 <$>  [0, (-250)..] <*> map (Text . fst) items
        in  pure $ translate (-50) 250 $ Pictures (menuSelector : labels)

instance Handle (Menu a) where
    handle e m@Menu{emptylist = True} = pure m
    handle e m@Menu{selected = s, maxSelected = ms} = pure $ case e of 
        EventKey (Char 'w') Down _ _ -> m { selected = max 0  $ s-1}
        EventKey (Char 's') Down _ _ -> m { selected = min ms $ s+1}
        _                            -> m

createMenu :: [(String, a -> a)] -> Menu a
createMenu [] = Menu [] 0 0 True
createMenu lst = Menu { menuItems = lst, maxSelected = length lst - 1, selected = 0, emptylist = False}

menuAction :: Menu a -> (a -> a)
menuAction Menu{emptylist = True}                  = id
menuAction Menu{menuItems = actions, selected = s} = map snd actions !! s
