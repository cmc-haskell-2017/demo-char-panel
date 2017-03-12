{-# LANGUAGE DeriveFunctor #-}
module CharSelect.Form where

import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe (maybe)
import Data.Monoid

data Field
  = FieldSlider   Slider
  | FieldSelecter Selecter

data Slider = Slider Float Bool

data Selecter = Selecter (Maybe Int)

type Fields = [(String, Field)]

data Form a = Form
  { formFields  :: Fields
  , formValue   :: Fields -> Either String a
  , formDraw    :: Fields -> Picture
  , formHandle  :: Event -> Fields -> Fields
  , formHeight  :: Float
  } deriving (Functor)

instance Applicative Form where
  pure x = Form [] (const (pure x)) (const blank) (const id) 0

  Form s1 v1 d1 h1 y1 <*> Form s2 v2 d2 h2 y2 = Form s v d h y
    where
      s = s1 <> s2
      v xs = v1 xs <*> v2 xs
      d xs = d1 xs <> translate 0 (- y1) (d2 xs)
      h e = h1 e . h2 (translateMouse 0 y1 e)
      y = y1 + y2

translateMouse :: Float -> Float -> Event -> Event
translateMouse dx dy (EventKey k ks m (x, y)) = (EventKey k ks m (x + dx, y + dy))
translateMouse dx dy (EventMotion (x, y)) = EventMotion (x + dx, y + dy)
translateMouse _ _ e = e

updateField :: String -> (Field -> Field) -> Fields -> Fields
updateField _ _ [] = []
updateField name g ((k, v) : xs)
  | k == name = (name, g v) : xs
  | otherwise = (k, v) : updateField name g xs

mkField
  :: String
  -> Field
  -> (Field -> Either String a)
  -> (Field -> Picture)
  -> (Event -> Field -> Field)
  -> Float
  -> Form a
mkField name field value draw handle height = Form
  { formFields = [(name, field)]
  , formValue  = maybe (Left "no such field") value . lookup name
  , formDraw   = foldMap draw . lookup name
  , formHandle = updateField name . handle
  , formHeight = height
  }

toSlider :: Field -> Either String Slider
toSlider (FieldSlider s) = Right s
toSlider _ = Left "not a slider"

mapSlider :: (Slider -> Slider) -> Field -> Field
mapSlider g (FieldSlider s) = FieldSlider (g s)
mapSlider _ f = f

sliderValue :: Slider -> Float
sliderValue (Slider x _) = x

slider :: String -> Form Float
slider name = limitValues (\x -> 0 <= x && x <= 1) $ mkField name
  (FieldSlider (Slider 0 False))
  (fmap sliderValue . toSlider)
  (foldMap drawSlider . toSlider)
  (mapSlider . handleSlider)
  50

drawForm :: Form a -> Picture
drawForm form = formDraw form (formFields form)

handleForm :: Event -> Form a -> Form a
handleForm e form = form
  { formFields = formHandle form e (formFields form) }

limitValues :: (a -> Bool) -> Form a -> Form a
limitValues p form = form { formHandle = handle }
  where
    handle e old
      | all p (formValue form new) = new
      | otherwise         = old
      where
        new = formHandle form e old

drawSlider :: Slider -> Picture
drawSlider (Slider x _) = pictures
  [ color (greyN 0.5) (line [ (-100, 0), (100, 0) ] )
  , color white (translate (200*x - 100) 0 (thickCircle (r/2) r))
  ]
  where
    r = 5

handleSlider :: Event -> Slider -> Slider
handleSlider (EventKey (MouseButton LeftButton) Down _ (x, y)) (Slider z _)
  | onCircle  = Slider z True
  where
    onCircle = (x - 200*z + 100)^2 + y^2 <= 5
handleSlider (EventKey (MouseButton LeftButton) Up _ (x, y)) (Slider z True) = Slider z False
handleSlider (EventMotion (x, _)) (Slider _ True) = Slider ((x + 100) / 200) True
handleSlider _ s = s

