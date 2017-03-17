module CharSelect where

import Graphics.Gloss.Interface.Pure.Game

import CharSelect.Panel

charSelectScreen :: IO ()
charSelectScreen =
  play display bgColor fps initScreen drawScreen handleScreen updateScreen
  where
    display = InWindow "Экран выбора персонажа" (screenWidth, screenHeight) (100, 100)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

type Screen = Panel Attrs

initScreen :: Screen
initScreen = validatePanel ((< 10) . attrsTotal) attrs

data Attrs = Attrs
  { attrStrength  :: Int
  , attrDexterity :: Int
  , attrVitality  :: Int
  , attrEnergy    :: Int
  }

attrsTotal :: Attrs -> Int
attrsTotal (Attrs s d v e) = s + d + v + e

attrs :: Panel Attrs
attrs = Attrs
  <$> slider "Strength"   0 10 orange
  <*> slider "Dexterity"  0 10 yellow
  <*> slider "Vitality"   0 10 red
  <*> slider "Energy"     0 10 blue

drawScreen :: Screen -> Picture
drawScreen = drawPanel

handleScreen :: Event -> Screen -> Screen
handleScreen = handlePanel

updateScreen :: Float -> Screen -> Screen
updateScreen _ = id

screenWidth :: Num a => a
screenWidth = 1200

screenHeight :: Num a => a
screenHeight = 675
