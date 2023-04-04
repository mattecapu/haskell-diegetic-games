import Cybercats
import PD
import PDCollusion
import Ultimatum
import GameHelper

-- manually compute fixpoints of best response functions for now
pd = \x y -> fst ((costate2payoff gamePD) ((x, y), ((), ())))
pdProfiles = [(s1, s2) | s1 <- (list :: [MovesPD]), s2 <- (list :: [MovesPD])]
eqPD = filter (\(s1, s2) -> let (p1, p2) = pd s1 s2 in p1 s1 && p2 s2) pdProfiles

pdcollusion = \s1 s2 -> fst ((costate2payoff gamePDCollusion) ((s1, s2), ((), ())))
eqPDCollusion = filter (\(s1, s2) -> let p = pdcollusion s1 s2 in p (s1, s2)) pdProfiles

ultimatum = \x y -> fst ((costate2payoff gameUltimatum) ((x, y), ()))
ultimatumProfiles = [(s1, s2) | s1 <- (list :: [MovesUltimatum1]), s2 <- (list :: [MovesUltimatum1 -> MovesUltimatum2])]
eqUltimatum = filter (\(s1, s2) -> let (p1, p2) = ultimatum s1 s2 in p1 s1 && p2 s2) ultimatumProfiles

main :: IO ()
main = do
  putStr "PD (Nash equilibrium):\n\t"
  putStr (concat (map show eqPD))
  putStr "\n\n"
  putStr "PD but the players can deviate jointly (Pareto equilibrium):\n\t"
  putStr (concat (map show eqPDCollusion))
  putStr "\n\n"
  putStr "Ultimatum game (Nash equilibrium):\n\t"
  putStr (concat (map show eqUltimatum))
  putStr "\n\n"
