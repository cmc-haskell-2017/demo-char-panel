module Game.Utils where

import Graphics.Gloss.Interface.Pure.Game

-- | Построить прямоугольник с заданными координатами
-- левой нижней и правой верхней вершин.
rect :: Point -> Point -> Picture
rect (l, b) (r, t) = polygon [ (l, b), (l, t), (r, t), (r, b) ]

-- | Изменить положение мыши.
modifyEventMouse :: (Point -> Point) -> Event -> Event
modifyEventMouse f (EventKey k ks m mouse) = EventKey k ks m (f mouse)
modifyEventMouse f (EventMotion mouse) = EventMotion (f mouse)
modifyEventMouse _ e = e

-- | Пересчитать положение мыши относительно заданной точки.
untranslateEvent :: Vector -> Event -> Event
untranslateEvent v = modifyEventMouse (subtract v)

-- | Пересчитать положение мыши для масштабированного объекта.
unscaleEvent :: Float -> Float -> Event -> Event
unscaleEvent sx sy = modifyEventMouse (\(x, y) -> (x / sx, y / sy))

-- | Прямоугольник с закруглёнными краями и границей заданной толщины.
roundedRect
  :: Color    -- ^ Цвет заливки.
  -> Color    -- ^ Цвет границы.
  -> Float    -- ^ Ширина прямоугольника.
  -> Float    -- ^ Высота прямоугольника.
  -> Float    -- ^ Радиус закругления.
  -> Float    -- ^ Толщина границы.
  -> Picture
roundedRect innerColor borderColor w h r d = pictures
  [ color innerColor inner
  , color borderColor border
  ]
  where
    border = pictures
      [ rect (-w/2 - d/2, -h/2 + r) (-w/2 + d/2, h/2 - r)
      , rect ( w/2 - d/2, -h/2 + r) ( w/2 + d/2, h/2 - r)
      , rect ( w/2 - r, -h/2 + d/2) (-w/2 + r, -h/2 - d/2)
      , rect ( w/2 - r,  h/2 + d/2) (-w/2 + r,  h/2 - d/2)
      , translate (-w/2 + r) ( h/2 - r) (rotate 270 cornerBorder)
      , translate (-w/2 + r) (-h/2 + r) (rotate 180 cornerBorder)
      , translate ( w/2 - r) (-h/2 + r) (rotate 90 cornerBorder)
      , translate ( w/2 - r) ( h/2 - r) cornerBorder
      ]

    inner = pictures
      [ rect (-w/2, -h/2 + r) (-w/2 + r,  h/2 - r)
      , rect ( w/2, -h/2 + r) ( w/2 - r,  h/2 - r)
      , rect (-w/2 + r, -h/2) ( w/2 - r, -h/2 + r)
      , rect (-w/2 + r,  h/2) ( w/2 - r,  h/2 - r)
      , rect (-w/2 + r, -h/2 + r) (w/2 - r, h/2 - r)
      , translate (-w/2 + r) ( h/2 - r) (rotate 270 corner)
      , translate (-w/2 + r) (-h/2 + r) (rotate 180 corner)
      , translate ( w/2 - r) (-h/2 + r) (rotate 90 corner)
      , translate ( w/2 - r) ( h/2 - r) corner
      ]

    corner = thickArc 0 90 (r/2) r
    cornerBorder = thickArc 0 90 r d

