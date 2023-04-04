module PD where

import Cybercats
import GameHelper

data MovesPD = Cooperate | Defect
instance Show (MovesPD) where
  show Cooperate = "Cooperate"
  show Defect = "Defect"

instance Listable (MovesPD) where
  list = [Cooperate, Defect]

payoffPD :: (MovesPD, MovesPD) -> (Double, Double)
payoffPD (Cooperate, Cooperate) = (2, 2)
payoffPD (Cooperate,    Defect) = (0, 3)
payoffPD (Defect,    Cooperate) = (3, 0)
payoffPD (Defect,       Defect) = (1, 1)

gamePD = ((argmax_player #--# argmax_player) *** nashator *** parardiff ((corner #^^# corner) >--> payoff2costate payoffPD)) >--> unitor
