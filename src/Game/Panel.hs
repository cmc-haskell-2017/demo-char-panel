{-# LANGUAGE DeriveFunctor #-}
module Game.Panel where

import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (maybe)
import Data.Monoid

import Game.Panel.Field
import Game.Utils

-- | Панель с полями ввода.
data Panel a = Panel
  { panelFields  :: Fields              -- ^ Поля ввода.
  , panelValue   :: Fields -> Maybe a   -- ^ Функция чтения поля.
  } deriving (Functor)

instance Applicative Panel where
  pure x = Panel [] (pure (pure x))

  Panel f1 v1 <*> Panel f2 v2 = Panel f v
    where
      f = f1 <> f2
      v fs = v1 fs <*> v2 fs

-- | Считать значение панели.
readPanel :: Panel a -> Maybe a
readPanel = panelValue <*> panelFields

-- | Определить высоту панели.
panelHeight :: Panel a -> Float
panelHeight panel = fromIntegral n * fieldHeight
  where
    n = length (panelFields panel)

-- | Имена полей панели.
panelFieldNames :: Panel a -> [String]
panelFieldNames = map fst . panelFields

-- | Создать панель с единственным полем.
mkField
  :: String             -- ^ Уникальное имя поля.
  -> Field              -- ^ Поле ввода.
  -> (Field -> Maybe a) -- ^ Функция чтения поля.
  -> Panel a
mkField name field value = Panel
  { panelFields = [(name, field)]
  , panelValue  = maybe Nothing value . lookup name
  }

-- | Создать панель с одним полем-слайдером.
slider
  :: String   -- ^ Имя поля.
  -> Int      -- ^ Минимальное значение.
  -> Int      -- ^ Максимальное значение.
  -> Color    -- ^ Цвет слайдера.
  -> Panel Int
slider name minValue maxValue c = mkField name
  (FieldSlider (initSlider minValue maxValue c))
  (fmap sliderValue . toSlider)

-- | Создать панель с одним полем выбора.
selector
  :: String           -- ^ Название поля.
  -> (a -> Picture)   -- ^ Функция отображения вариантов.
  -> [a]              -- ^ Список вариантов.
  -> Panel a
selector name drawValue values = mkField name
  (FieldSelector (initSelector (map drawValue values)))
  (fmap (\s -> values !! selectorIndex s) . toSelector)

-- | Отобразить панель.
drawPanel
  :: (String -> Picture)  -- ^ Функция отрисовки имён полей.
  -> Panel a              -- ^ Панель.
  -> Picture
drawPanel drawFieldName panel = pictures
  [ drawPanelBackground (panelHeight panel)
  , pictures (mapNamedFieldsWithOffset (drawFieldWithOffset drawFieldName) panel)
  ]

-- | Отобразить рамку и фон панели.
drawPanelBackground :: Float -> Picture
drawPanelBackground h = translate (-0.3 * fw) (fh/2 - h/2)
  (roundedRect (withAlpha 0.7 white) (greyN 0.7) (2 * fw) (h + fh) (0.1 * fw) (0.02 * fw))
  where
    fh = fieldHeight
    fw = fieldWidth

-- | Отобразить поле с заданным отступом.
drawFieldWithOffset
  :: (String -> Picture)  -- ^ Функция отрисовки имени поля.
  -> Float                -- ^ Отступ по вертикали.
  -> NamedField           -- ^ Именованное поле.
  -> Picture
drawFieldWithOffset drawFieldName offsetY = translate 0 offsetY . drawNamedField drawFieldName

-- | Обработка событий панели.
handlePanel :: Event -> Panel a -> Panel a
handlePanel e panel = case panelValue panel newFields of
  Nothing -> panel
  Just _  -> panel { panelFields = newFields }
  where
    newFields = mapNamedFieldsWithOffset (fmap . handleFieldWithOffset e) panel

-- | Обработать каждое поле с его отступом по вертикали.
mapNamedFieldsWithOffset :: (Float -> NamedField -> a) -> Panel b -> [a]
mapNamedFieldsWithOffset f = zipWith f [0, -fieldHeight..] . panelFields

-- | Добавить ограничение на возможные значения панели.
constrainPanel :: (a -> Bool) -> Panel a -> Panel a
constrainPanel p panel = panel { panelValue = newValue }
  where
    newValue fs = case panelValue panel fs of
      Just x | p x -> Just x
      _ -> Nothing

-- | Обработать событие для одного поля с заданным отступом.
handleFieldWithOffset :: Event -> Float -> Field -> Field
handleFieldWithOffset e offsetY = handleField (untranslateEvent (0, offsetY) e)
