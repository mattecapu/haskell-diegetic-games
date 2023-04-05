module GameHelper(module Listable, module Cybercats, module GameHelper) where

import Listable
import Cybercats

type Pred x = x -> Bool

fixpoints :: (Listable x) => (x -> Pred x) -> [x]
fixpoints f = filter (\x -> (f x) x) list

argmax :: (Ord a, Eq a, Listable x) => (x -> a) -> (x -> Bool)
argmax k x = k x == maximum (map k list)

argmax_player' :: (Ord a, Eq a, Listable x) => Lens x (Pred x) x (Payoff x a)
argmax_player' = MkLens (id, \p k -> argmax (costate2fun k))

corner :: ParaLens p q () () p q
corner = MkLens (\(p, ()) -> p, \(p, ()) r -> (r, ()))

-- take_payoff_coplay :: (p, x) -> (me, you) -> (me, you)
-- take_payoff_coplay _ = id

-- take_payoff_coplay' :: (p, x) -> (you, me) -> (me, you)
-- take_payoff_coplay' _ (you, me) = (me, you)

eval_play :: (x -> y, x) -> y
eval_play (p, x) = p x

-- id_coplay :: (p, x) -> r -> ((), r)
-- id_coplay _ r = ((), r)

runitor :: Lens () (Payoff () ()) () ()
runitor = MkLens (id, \_ _ -> fun2costate id)

lunitor :: Lens () () () (Payoff () ())
lunitor = MkLens (id, \_ _ -> ())

type Player profiles utility actions = Lens profiles (Pred profiles) actions (Payoff actions utility)
type Arena actions utility states copayoffs moves payoffs = ParaLens actions utility states copayoffs moves payoffs

newtype Game profiles actions utility states copayoffs moves payoffs = MkGame (Player profiles utility actions, Arena actions utility states copayoffs moves payoffs)

oplaxator :: Lens (x, x') (Pred (x, x')) (x, x') (Pred x, Pred x')
oplaxator = MkLens (id, \_ (p1, p2) (x, x') -> p1 x && p2 x')

infixr 4 ##
(##) :: Player s u a -> Player s' u' a' -> Player (s, s') (u, u') (a, a')
player ## player' = oplaxator >--> (player #--# player') >--> nashator

argmax_player :: (Ord u, Eq u, Listable s) => Player s u s
argmax_player = MkLens (id, \p k -> argmax (costate2fun k))

newtype Context x s y r = MkContext (Point x s, Payoff y r)

packup :: Arena a u x s y r -> Context x s y r -> ParaLens a (Payoff a u) () () () ()
packup arena (MkContext (x, k)) = lunitor >-^> parardiff (x >-^> arena >^-> k) >^-> runitor

equilibria :: (Listable p) => Game p a u x s y r -> Context x s y r -> [p]
equilibria (MkGame (player, arena)) ctx = let game = player *** (packup arena ctx)  in fixpoints (parascalar2fun game)
