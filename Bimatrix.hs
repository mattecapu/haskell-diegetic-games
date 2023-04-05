{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Bimatrix where

import GameHelper

type TwoDoubles = (Double, Double)

instance {-# OVERLAPPING #-} Ord (TwoDoubles) where
  (<=) = \(x, y) (x', y') -> x <= x' && y <= y'
  (<)  = \(x, y) (x', y') -> x < x' && y < y'

type Bimatrix moves1 moves2 = (moves1, moves2) -> TwoDoubles

bimatrixArena :: Arena (moves1, moves2) TwoDoubles () () (moves1, moves2) TwoDoubles
bimatrixArena = (MkLens (\_ -> ((), ()), \_ _ -> ())) >-^> (corner #^^# corner)

bimatrixContext :: Bimatrix moves1 moves2 -> Context () () (moves1, moves2) TwoDoubles
bimatrixContext k = MkContext (point2state (), fun2costate k)

bimatrixGame :: (Listable moves1, Listable moves2) => Bimatrix moves1 moves2 -> Game (moves1, moves2) (moves1, moves2) TwoDoubles () () (moves1, moves2) TwoDoubles
bimatrixGame bimatrix = MkGame (argmax_player ## argmax_player, bimatrixArena)
