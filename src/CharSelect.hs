module CharSelect where

import Graphics.Gloss.Interface.Pure.Game

import CharSelect.Form

charSelectScreen :: IO ()
charSelectScreen =
  play display bgColor fps initScreen drawScreen handleScreen updateScreen
  where
    display = InWindow "Экран выбора персонажа" (screenWidth, screenHeight) (100, 100)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

type Screen = Form Float

initScreen :: Screen
initScreen = slider "ololo" <* slider "alala" <* slider "elele"

drawScreen :: Screen -> Picture
drawScreen = drawForm

handleScreen :: Event -> Screen -> Screen
handleScreen = handleForm

updateScreen :: Float -> Screen -> Screen
updateScreen _ = id

screenWidth :: Num a => a
screenWidth = 1200

screenHeight :: Num a => a
screenHeight = 675
