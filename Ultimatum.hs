module Ultimatum where

import GameHelper

data MovesUltimatum1 = Fair | Unfair deriving Eq
instance Show (MovesUltimatum1) where
  show Fair   = "Fair"
  show Unfair = "Unfair"

data MovesUltimatum2 = Accept | Reject deriving Eq
instance Show (MovesUltimatum2) where
  show Accept = "Accept"
  show Reject = "Reject"

instance Listable (MovesUltimatum1) where
  list = [Fair, Unfair]

instance Listable (MovesUltimatum2) where
  list = [Accept, Reject]

payoffUltimatum :: (MovesUltimatum1, MovesUltimatum2) -> (Double, Double)
payoffUltimatum (Fair,   Accept) = (5, 5)
payoffUltimatum (Fair,   Reject) = (0, 0)
payoffUltimatum (Unfair, Accept) = (8, 2)
payoffUltimatum (Unfair, Reject) = (0, 0)

first_stage :: ParaLens MovesUltimatum1 Double () () MovesUltimatum1 Double
first_stage = corner

interlude :: Lens x r (x, x) ((), r)
interlude = MkLens (\x -> (x, x), \_ ((), r) -> r)

second_stage :: ParaLens (MovesUltimatum1 -> MovesUltimatum2) Double MovesUltimatum1 () MovesUltimatum2 Double
second_stage = MkLens (eval_play, \_ r -> (r, ()))

arenaUltimatum = (first_stage >^-> interlude) >^^> (second_stage #^-# idlens) >^-> exchange

gameUltimatum = (argmax_player' #--# argmax_player') *** nashator *** parardiff (arenaUltimatum >^-> (fun2costate (payoffUltimatum))) >--> runitor
