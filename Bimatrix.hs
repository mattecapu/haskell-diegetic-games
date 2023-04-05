module Bimatrix where

import GameHelper

type Bimatrix moves1 moves2 = (moves1, moves2) -> (Double, Double)

bimatrixArena :: Arena (moves1, moves2) (Double, Double) () () (moves1, moves2) (Double, Double)
bimatrixArena = (MkLens (\_ -> ((), ()), \_ _ -> ())) >-^> (corner #^^# corner)

bimatrixContext :: Bimatrix moves1 moves2 -> Context () () (moves1, moves2) (Double, Double)
bimatrixContext k = MkContext (point2state (), fun2costate k)

bimatrixGame :: (Listable moves1, Listable moves2) => Bimatrix moves1 moves2 -> Game (moves1, moves2) (moves1, moves2) (Double, Double) () () (moves1, moves2) (Double, Double)
bimatrixGame bimatrix = MkGame (argmax_player ## argmax_player, bimatrixArena)
