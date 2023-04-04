import Cybercats
import PD
import PDCollusion

pd = \x y -> fst ((costate2payoff gamePD) ((x, y), ((), ())))
eqPD = filter (\(x, y) -> let (p1, p2) = pd Cooperate Cooperate in p1 x && p2 y) [ (x, y) | x <- [Cooperate, Defect], y <- [Cooperate, Defect]]

pdcollusion = \x y -> fst ((costate2payoff gamePDCollusion) ((x, y), ((), ())))
eqPDCollusion = filter (\(x, y) -> let p = pdcollusion Cooperate Cooperate in p (x, y)) [ (x, y) | x <- [Cooperate, Defect], y <- [Cooperate, Defect]]

main :: IO ()
main = do
  putStr "PD (Nash equilibrium):\n\t"
  putStr (concat (map show eqPD))
  putStr "\n\n"
  putStr "PD but the players can deviate jointly (Pareto equilibrium):\n\t"
  putStr (concat (map show eqPDCollusion))
  putStr "\n\n"
