module GameHelper(module Listable, module Cybercats, module GameHelper) where

import Listable
import Cybercats

type Pred x = x -> Bool

argmax :: (Ord a, Eq a, Listable x) => (x -> a) -> (x -> Bool)
argmax k x = k x == maximum (map k list)

argmax_player :: (Ord a, Eq a, Listable x) => Lens x (Pred x) x (Payoff x a)
argmax_player = MkLens (id, \p k -> argmax (costate2payoff k))

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

unitor :: Lens () (Payoff () ()) () ()
unitor = MkLens (id, \_ _ -> payoff2costate id)
