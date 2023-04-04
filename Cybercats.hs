module Cybercats where

newtype Lens x s y r = MkLens (x -> y, x -> r -> s)
type Payoff x s = Lens x s () ()
type Point x s = Lens () () x s

-- diagrammatic composition of lenses
infixr 4 >>>>
(>>>>) :: Lens x s y r -> Lens y r z t -> Lens x s z t
(MkLens (play, coplay)) >>>> (MkLens (play', coplay')) = MkLens (play' . play, \x t -> coplay x (coplay' (play x) t))

-- 'reverse derivative' of lenses
rdiff :: Lens x s y r -> Lens x (Payoff x s) y (Payoff y r)
rdiff lens = let MkLens (play, coplay) = lens in MkLens (play, \x k -> lens >>>> k)

-- monoidal product of lenses
infixr 4 #
(#) :: Lens x s y r -> Lens x' s' y' r' -> Lens (x, x') (s, s') (y, y') (r, r')
(MkLens (play, coplay)) # (MkLens (play', coplay')) = MkLens (\ (x, x') -> (play x, play' x'), \ (x, x') (r, r') -> (coplay x r, coplay' x' r'))

-- point2lens :: x -> Point x s
-- point2lens x = MkLens(\() -> x, \() s -> ())

costate2payoff :: Payoff x s -> (x -> s)
costate2payoff (MkLens (_, coplay)) x = coplay x ()

payoff2costate :: (x -> s) -> Payoff x s
payoff2costate k = MkLens (\x -> (), \x () -> k x)

-- the relevant part of the nashator without the lensy overhead
nashator_sharp :: (x, x') -> ((x, x') -> (s, s')) -> (x -> s, x' -> s')
nashator_sharp (x0, x0') k = (\x -> fst (k (x, x0')), \x' -> snd (k (x0, x')))

nashator :: Lens (x, x') (Payoff x s, Payoff x' s') (x, x') (Payoff (x, x') (s, s'))
nashator = MkLens (id, \xx0 k -> let (k1, k2) = nashator_sharp xx0 (costate2payoff k) in (payoff2costate k1, payoff2costate k2))

-- parametric lenses
type ParaLens p q x s y r = Lens (p, x) (q, s) y r

-- sequential composition of parametric lenses
(>>>>>) :: ParaLens p q x s y r -> ParaLens p' q' y r z t -> ParaLens (p, p') (q, q') x s z t
(MkLens (play, coplay)) >>>>> (MkLens (play', coplay')) = MkLens (
        \((p, p'), x) -> play'(p', play(p, x)),
        \((p, p'), x) t ->  let (q', r) = coplay' (p', play (p, x)) t
                                (q, s) = coplay (p, x) r
                            in ((q, q'), s)
    )

-- rearranges 'parameters' and 'states' after a monoidal product
interchanger :: Lens ((p, x), (p', x')) ((q, s), (q', s')) ((p, p'), (x, x')) ((q, q'), (s, s'))
interchanger = MkLens (\((p, x), (p', x')) -> ((p, p'), (x, x')), \_ ((q, q'), (s, s')) -> ((q, s), (q', s')))

-- monoidal product of parametric lenses
infixr 4 ##
(##) :: ParaLens p q x s y r -> ParaLens p' q' x' s' y' r' -> ParaLens (p, p') (q, q') (x, x') (s, s') (y, y') (r, r')
top ## bottom = interchanger >>>> top # bottom

idlens :: Lens x s x s
idlens = MkLens (\x -> x, \x s -> s)

-- reparameterization
infixr 5 ***
(***) :: Lens p' q' p q -> ParaLens p q x s y r -> ParaLens p' q' x s y r
r *** plens = (r # idlens) >>>> plens

-- 'reverse derivative' of parametric lenses
parardiff :: ParaLens p q x s y r -> ParaLens p (Payoff p q) x (Payoff x s) y (Payoff y r)
parardiff plens = nashator >>>> rdiff plens

corner :: ParaLens p q () () p q
corner = MkLens (\(p, ()) -> p, \(p, ()) r -> (r, ()))

-- lunitor :: Lens x s (x, ()) (s, ())
-- lunitor = MkLens (\x -> (x, ()), \x (s, ()) -> s)

-- runitor :: Lens x s ((), x) ((), s)
-- runitor = MkLens (\x -> ((), x), \x ((), s) -> s)

type Pred x = x -> Bool

class Listable x where
    list :: [x]

instance (Listable x, Listable y) => Listable (x, y) where
    list = [(a, b) | a <- list, b <- list]

argmax :: (Ord a, Eq a, Listable x) => (x -> a) -> (x -> Bool)
argmax k x = k x == maximum (map k list)

argmax_player :: (Ord a, Eq a, Listable x) => Lens x (Pred x) x (Payoff x a)
argmax_player = MkLens (id, \p k -> argmax (costate2payoff k))
