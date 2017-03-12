module CharSelect where

import Graphics.Gloss.Interface.Pure.Game

charSelectScreen :: IO ()
charSelectScreen =
  play display bgColor fps initScreen drawScreen handleScreen updateScreen
  where
    display = InWindow "Экран выбора персонажа" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

data Screen = Screen

initScreen :: Screen
initScreen = Screen

drawScreen :: Screen -> Picture
drawScreen _ = blank

handleScreen :: Event -> Screen -> Screen
handleScreen _ = id

updateScreen :: Float -> Screen -> Screen
updateScreen _ = id

screenWidth :: Num a => a
screenWidth = 800

screenHeight :: Num a => a
screenHeight = 450
