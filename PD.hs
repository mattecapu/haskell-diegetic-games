module PD where

import Cybercats

data MovesPD = Cooperate | Defect
instance Show (MovesPD) where
  show Cooperate = "Cooperate"
  show Defect = "Defect"

instance Listable (MovesPD) where
  list = [Cooperate, Defect]

prisoner :: Lens MovesPD (Pred MovesPD) MovesPD (Payoff MovesPD Double)
prisoner = argmax_player -- MkLens (id, \p k -> argmax (costate2payoff k) [Cooperate, Defect])

payoffPD :: (MovesPD, MovesPD) -> (Double, Double)
payoffPD (Cooperate, Cooperate) = (2, 2)
payoffPD (Cooperate,    Defect) = (0, 3)
payoffPD (Defect,    Cooperate) = (3, 0)
payoffPD (Defect,       Defect) = (1, 1)

arenaPD :: ParaLens (MovesPD, MovesPD) (Double, Double) ((), ()) ((), ()) () ()
arenaPD = (corner ## corner) >>>> payoff2costate payoffPD

unitor :: Lens () (Payoff () ()) () ()
unitor = MkLens (id, \_ _ -> payoff2costate id)

gamePD = ((prisoner # prisoner) *** nashator *** parardiff arenaPD) >>>> unitor
