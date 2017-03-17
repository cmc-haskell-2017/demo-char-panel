module CharSelect.Character where

data Race
  = Human
  | Elf
  | Orc
  deriving (Show, Bounded, Enum)

data Class
  = Warrior
  | Hunter
  | Priest
  | Mage
  deriving (Show, Bounded, Enum)

data Character = Character
  { charName      :: String
  , charRace      :: Race
  , charClass     :: Class
  , charSkinTone  :: Float
  , charAttrs     :: Attrs
  }

data Attrs = Attrs
  { attrStrength  :: Int
  , attrDexterity :: Int
  , attrVitality  :: Int
  , attrEnergy    :: Int
  }

