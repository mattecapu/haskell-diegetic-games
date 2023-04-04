module GameHelper where

import Cybercats

type Pred x = x -> Bool

class Listable x where
    list :: [x]

instance (Listable x, Listable y) => Listable (x, y) where
    list = [(a, b) | a <- list, b <- list]

argmax :: (Ord a, Eq a, Listable x) => (x -> a) -> (x -> Bool)
argmax k x = k x == maximum (map k list)

argmax_player :: (Ord a, Eq a, Listable x) => Lens x (Pred x) x (Payoff x a)
argmax_player = MkLens (id, \p k -> argmax (costate2payoff k))

corner :: ParaLens p q () () p q
corner = MkLens (\(p, ()) -> p, \(p, ()) r -> (r, ()))

unitor :: Lens () (Payoff () ()) () ()
unitor = MkLens (id, \_ _ -> payoff2costate id)
