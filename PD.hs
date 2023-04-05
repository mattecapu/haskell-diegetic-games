module PD where

import GameHelper
import Bimatrix

data MovesPD = Cooperate | Defect
instance Show (MovesPD) where
  show Cooperate = "Cooperate"
  show Defect = "Defect"

instance Listable (MovesPD) where
  list = [Cooperate, Defect]

payoffPD :: (MovesPD, MovesPD) -> TwoDoubles
payoffPD (Cooperate, Cooperate) = (2, 2)
payoffPD (Cooperate,    Defect) = (0, 3)
payoffPD (Defect,    Cooperate) = (3, 0)
payoffPD (Defect,       Defect) = (1, 1)

gamePD = bimatrixGame payoffPD
equilibriaPD = equilibria gamePD (bimatrixContext payoffPD)
