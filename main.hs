import PD
import PDCollusion
import Ultimatum
import GameHelper

-- manually compute fixpoints of best response functions for now
ultimatum = \x y -> fst ((costate2fun gameUltimatum) ((x, y), ()))
ultimatumProfiles = [(s1, s2) | s1 <- (list :: [MovesUltimatum1]), s2 <- (list :: [MovesUltimatum1 -> MovesUltimatum2])]
eqUltimatum = filter (\(s1, s2) -> let (p1, p2) = ultimatum s1 s2 in p1 s1 && p2 s2) ultimatumProfiles

main :: IO ()
main = do
  putStr "PD (Nash equilibrium):\n\t"
  putStr (concat (map show equilibriaPD))
  putStr "\n\n"
  putStr "PD but the players can deviate jointly (Pareto equilibrium):\n\t"
  putStr (concat (map show equilibriaPDCollusion))
  putStr "\n\n"
  putStr "Ultimatum game (Nash equilibrium):\n\t"
  putStr (concat (map show eqUltimatum))
  putStr "\n\n"
