module PDCollusion where

import GameHelper
import PD
import Bimatrix

colluding_player :: Player (MovesPD, MovesPD) TwoDoubles (MovesPD, MovesPD)
colluding_player = argmax_player

gamePDCollusion = MkGame (colluding_player, bimatrixArena)
equilibriaPDCollusion = equilibria gamePDCollusion (bimatrixContext payoffPD)
