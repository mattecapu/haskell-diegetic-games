module PDCollusion where

import Cybercats
import PD

newtype TwoDoubles = MkTwo (Double, Double) deriving (Eq)

instance Ord (TwoDoubles) where
  (<=) = \(MkTwo (x, y)) (MkTwo (x', y')) -> x <= x' && y <= y'
  (<) = \(MkTwo (x, y)) (MkTwo (x', y')) -> x < x' && y < y'

totwodoubles :: Lens x TwoDoubles x (Double, Double)
totwodoubles = MkLens (id, \_ (x,y) -> MkTwo (x, y))

gamePDCollusion = (argmax_player *** parardiff (totwodoubles *** arenaPD)) >>>> unitor