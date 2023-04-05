module Listable where

class Listable x where
    list :: [x]

instance (Listable x, Listable y) => Listable (x, y) where
    list = [(a, b) | a <- list, b <- list]

buildfuns :: Eq x => [x] -> [y] -> [x -> y]
buildfuns [] ys = [\_ -> undefined]
buildfuns (x : xs) ys = [ext f x y | f <- buildfuns xs ys, y <- ys]

ext :: (Eq x) => (x -> y) -> x -> y -> x -> y
ext f x y x'
  | x == x'   = y
  | otherwise = f x'

instance (Eq x, Listable x, Listable y) => Listable (x -> y) where
  list = buildfuns list list

instance (Eq x, Listable x, Listable y, Show x, Show y) => Show (x -> y) where
  show f = let ss = [show x ++ " -> " ++ show (f x) | x <- list] in foldl (\x y -> x ++ ", " ++ y) (head ss) (tail ss)
