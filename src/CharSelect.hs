module CharSelect where

import Data.Monoid
import Data.Foldable
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

import CharSelect.Character
import CharSelect.Panel

charSelectScreen :: IO ()
charSelectScreen = do
  screen <- initScreen
  play display bgColor fps screen drawScreen handleScreen updateScreen
  where
    display = InWindow "Экран выбора персонажа" (screenWidth, screenHeight) (100, 100)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

data Images = Images
  { imgBackground :: Picture
  , imgFieldName  :: String -> Picture
  , imgSexName    :: Sex -> Picture
  , imgRaceName   :: Race -> Picture
  , imgClassName  :: Class -> Picture
  , imgCharType   :: CharType -> Picture
  }

loadImages :: IO Images
loadImages = Images
  <$> fmap fold (loadJuicyJPG "images/background.jpeg")
  <*> loadAll loadTextImage   allFieldNames
  <*> loadAll sexNameImage    allSexes
  <*> loadAll raceNameImage   allRaces
  <*> loadAll classNameImage  allClasses
  <*> loadAll charTypeImage   allCharTypes

loadTextImage :: String -> IO (Maybe Picture)
loadTextImage s = fmap (fmap (translate 0 10 . scale 0.14 0.14)) (loadJuicyPNG path)
  where
    path = "images/" ++ s ++ ".png"

allFieldNames :: [String]
allFieldNames = panelFieldNames (character (Images mempty mempty mempty mempty mempty mempty))

loadAll :: Eq a => (a -> IO (Maybe Picture)) -> [a] -> IO (a -> Picture)
loadAll load xs = indexEnum <$> sequenceA (map load xs)
  where
    indexEnum ps x = case lookup x (zip xs ps) of
      Nothing -> blank
      Just mp -> fold mp

sexNameImage :: Sex -> IO (Maybe Picture)
sexNameImage = loadTextImage . show

raceNameImage :: Race -> IO (Maybe Picture)
raceNameImage = loadTextImage . show

classNameImage :: Class -> IO (Maybe Picture)
classNameImage = loadTextImage . show

charTypeImage :: CharType -> IO (Maybe Picture)
charTypeImage ct = loadJuicyPNG path
  where
    path = "images/" ++ show (charSex ct) ++ show (charRace ct) ++ show (charClass ct) ++ ".png"

data Screen = Screen
  { screenPanel   :: Panel Character
  , screenChar    :: Maybe Character
  , screenImages  :: Images
  }

updateScreenPanel :: (Panel Character -> Panel Character) -> Screen -> Screen
updateScreenPanel f screen = screen
  { screenPanel = newPanel
  , screenChar  = readPanel newPanel
  }
  where
    newPanel = f (screenPanel screen)

initScreen :: IO Screen
initScreen = do
  images <- loadImages
  return Screen
    { screenPanel   = character images
    , screenChar    = readPanel (character images)
    , screenImages  = images
    }

character :: Images -> Panel Character
character images = Character
  <$> characterType images
  <*> skinTone
  <*> attrs

characterType :: Images -> Panel CharType
characterType images = CharType
  <$> selector "Sex"    (imgSexName   images) allSexes
  <*> selector "Race"   (imgRaceName  images) allRaces
  <*> selector "Class"  (imgClassName images) allClasses

skinTone :: Panel Float
skinTone = fmap g (slider "Skin tone" 0 n (greyN 0.5))
  where
    n = 1000
    g i = fromIntegral i / fromIntegral n

attrsTotal :: Attrs -> Int
attrsTotal (Attrs s d v e) = s + d + v + e

attrs :: Panel Attrs
attrs = validatePanel ((<= 10) . attrsTotal) attrsPanel
  where
    attrsPanel = Attrs
      <$> slider "Strength"   0 10 orange
      <*> slider "Dexterity"  0 10 yellow
      <*> slider "Vitality"   0 10 red
      <*> slider "Energy"     0 10 blue

drawScreen :: Screen -> Picture
drawScreen screen = pictures
  [ scale (1200 / 1920) (1200 / 1920) (imgBackground (screenImages screen))
  , uncurry translate charOffset (foldMap drawCharacter (screenChar screen))
  , uncurry translate panelOffset (drawPanel (imgFieldName (screenImages screen)) (screenPanel screen))
  ]
  where
    drawCharacter c = scale charSize charSize (drawSkin c <> drawCharType c)
    drawCharType = imgCharType (screenImages screen) . charType
    drawSkin c = color (charSkinColor c)
      (polygon [ (-290, -390), (-290, 310), (290, 310), (290, -390) ])

charSkinColor :: Character -> Color
charSkinColor c = mixColors (1 - t) t darkSkinColor lightSkinColor
  where
    t = charSkinTone c
    (darkSkinColor, lightSkinColor) = raceSkinColorRange (charRace (charType c))

raceSkinColorRange :: Race -> (Color, Color)
raceSkinColorRange Human = (makeColorI 255 219 172 255, makeColorI 141 85 36 255)
raceSkinColorRange Elf   = (makeColorI 219 172 255 255, makeColorI 85 36 141 255)
raceSkinColorRange Orc   = (makeColorI 219 255 172 255, makeColorI 85 141 36 255)

handleScreen :: Event -> Screen -> Screen
handleScreen = updateScreenPanel . handlePanel . uncurry translateMouse (- panelOffset)

updateScreen :: Float -> Screen -> Screen
updateScreen _ = id

screenWidth :: Num a => a
screenWidth = 1200

screenHeight :: Num a => a
screenHeight = 650

charSize :: Float
charSize = 1200 / 1920

panelOffset :: (Float, Float)
panelOffset = (-fieldWidth / 2, (panelHeight (character undefined) - fieldHeight) / 2)

charOffset :: (Float, Float)
charOffset = (fieldWidth, 0)

