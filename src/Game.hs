module Game where

import Data.Monoid
import Data.Foldable
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

import Game.Character
import Game.Panel
import Game.Panel.Field (fieldWidth, fieldHeight)
import Game.Utils

-- | Запустить экран выбора персонажа.
characterScreen :: IO ()
characterScreen = do
  screen <- initScreen
  play display bgColor fps screen drawScreen handleScreen updateScreen
  where
    display = InWindow "Экран выбора персонажа" (screenWidth, screenHeight) (100, 100)
    bgColor = white   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Изображения.
data Images = Images
  { imgBackground :: Picture              -- ^ Фон.
  , imgFieldName  :: String -> Picture    -- ^ Изображения имён полей.
  , imgSexName    :: Sex -> Picture       -- ^ Изображения полов.
  , imgRaceName   :: Race -> Picture      -- ^ Изображения рас.
  , imgClassName  :: Class -> Picture     -- ^ Изображения классов.
  , imgCharType   :: CharType -> Picture  -- ^ Изображения разных типов персонажей.
  }

-- | Загрузка изображений.
loadImages :: IO Images
loadImages = Images
  <$> fmap fold (loadJuicyJPG "images/background.jpeg")
  <*> loadAll loadTextImage   allFieldNames
  <*> loadAll sexNameImage    allSexes
  <*> loadAll raceNameImage   allRaces
  <*> loadAll classNameImage  allClasses
  <*> loadAll charTypeImage   allCharTypes

-- | Загрузка изображения для надписи.
loadTextImage :: String -> IO (Maybe Picture)
loadTextImage s = fmap (translate 0 10 . scale 0.14 0.14)
  <$> loadJuicyPNG path
  where
    path = "images/" ++ s ++ ".png"

-- | Список всех имён полей для панели настроек персонажа.
allFieldNames :: [String]
allFieldNames = panelFieldNames (character (Images mempty mempty mempty mempty mempty mempty))

-- | Загрузить все изображения для списка значений.
loadAll :: Eq a => (a -> IO (Maybe Picture)) -> [a] -> IO (a -> Picture)
loadAll load xs = indexEnum <$> sequenceA (map load xs)
  where
    indexEnum ps x = case lookup x (zip xs ps) of
      Nothing -> blank
      Just mp -> fold mp

-- | Загрузить изображение пола персонажа.
sexNameImage :: Sex -> IO (Maybe Picture)
sexNameImage = loadTextImage . show

-- | Загрузить изображение расы персонажа.
raceNameImage :: Race -> IO (Maybe Picture)
raceNameImage = loadTextImage . show

-- | Загрузить изображение класса персонажа.
classNameImage :: Class -> IO (Maybe Picture)
classNameImage = loadTextImage . show

-- | Загрузить изображение персонажа.
charTypeImage :: CharType -> IO (Maybe Picture)
charTypeImage ct = loadJuicyPNG path
  where
    path = "images/" ++ show (charSex ct) ++ show (charRace ct) ++ show (charClass ct) ++ ".png"

-- | Экран выбора персонажа.
data Screen = Screen
  { screenPanel   :: Panel Character  -- ^ Панель с настройками.
  , screenChar    :: Maybe Character  -- ^ Персонаж.
  , screenImages  :: Images           -- ^ Изображения.
  }

-- | Обновить панель на экране.
updateScreenPanel :: (Panel Character -> Panel Character) -> Screen -> Screen
updateScreenPanel f screen = screen
  { screenPanel = newPanel
  , screenChar  = readPanel newPanel
  }
  where
    newPanel = f (screenPanel screen)

-- | Инициализировать экран.
-- При инициализации загружаются все необходимые изображения.
initScreen :: IO Screen
initScreen = screenWithImages <$> loadImages

-- | Инициализировать экран с заданными изображениями.
screenWithImages :: Images -> Screen
screenWithImages images = Screen
  { screenPanel   = character images
  , screenChar    = readPanel (character images)
  , screenImages  = images
  }

--------------------------------------------------------------------------------
-- * Задание
--
-- Измените реализацию 'character', чтобы включать
-- поля пола, класса, настройки цвета кожи, силы и ловкости.
--
-- Для этого реализуйте подпанели 'characterType', 'skinTone' и 'attrs',
-- типы для которых объявлены ниже.
--------------------------------------------------------------------------------

-- | Панель настроек персонажа.
character :: Images -> Panel Character
character images = mkCharacter
  <$> selector "Race" (imgRaceName images) allRaces
  <*> slider "Vitality" 0 10 red
  <*> slider "Energy"   0 10 blue
  where
    mkCharacter race vitality energy = Character
      { charType     = CharType Male race NoClass
      , charSkinTone = 0
      , charAttrs    = Attrs 0 0 vitality energy
      }

-- | Настройки пола, расы и класса.
characterType :: Images -> Panel CharType
-- реализуйте самостоятельно
-- используйте имена полей "Sex", "Race" и "Class"
characterType images = pure (CharType Male Human NoClass)

-- | Поле настройки цвета кожи.
-- Поле может иметь значение от 0 до 1.
skinTone :: Panel Float
-- реализуйте самостоятельно
-- используйте имя поля "Skin tone"
skinTone = pure 0

-- | Настройки атрибутов персонажа (сила, ловкость, здоровье и энергия).
-- Сумма значений атрибутов не может превышать 'maxAttrsTotal'.
attrs :: Panel Attrs
attrs = constrainPanel ((<= maxAttrsTotal) . attrsTotal) attrsPanel
  where
    -- реализуйте самостоятельно
    -- используйте имена полей "Strength", "Dexterity", "Vitality" и "Energy"
    attrsPanel = pure (Attrs 0 0 0 0)

--------------------------------------------------------------------------------

-- | Отрисовка экрана выбора персонажа.
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

-- | Цвет персонажа.
charSkinColor :: Character -> Color
charSkinColor c = mixColors (1 - t) t darkSkinColor lightSkinColor
  where
    t = charSkinTone c
    (darkSkinColor, lightSkinColor) = raceSkinColorRange (charRace (charType c))

-- | Диапазон цветов кожи для каждой расы.
raceSkinColorRange :: Race -> (Color, Color)
raceSkinColorRange Human = (makeColorI 255 219 172 255, makeColorI 141 85 36 255)
raceSkinColorRange Elf   = (makeColorI 219 172 255 255, makeColorI 85 36 141 255)
raceSkinColorRange Orc   = (makeColorI 219 255 172 255, makeColorI 85 141 36 255)

-- | Обработка событий экрана выбора персонажа.
handleScreen :: Event -> Screen -> Screen
handleScreen = updateScreenPanel . handlePanel . untranslateEvent panelOffset

-- | Обновление экрана.
-- Поскольку все изменения происходят по событиям,
-- эта функция ничего не делает.
updateScreen :: Float -> Screen -> Screen
updateScreen _ = id

-- | Ширина экрана.
screenWidth :: Num a => a
screenWidth = 1200

-- | Высота экрана.
screenHeight :: Num a => a
screenHeight = 650

-- | Масштаб отрисовки персонажа.
charSize :: Float
charSize = 1200 / 1920

-- | Положение панели настроек.
panelOffset :: (Float, Float)
panelOffset = (-fieldWidth / 2, (panelHeight (character undefined) - fieldHeight) / 2)

-- | Положение персонажа.
charOffset :: (Float, Float)
charOffset = (300, 0)

-- | Максимальное суммарное значение атрибутов.
maxAttrsTotal :: Int
maxAttrsTotal = 10
