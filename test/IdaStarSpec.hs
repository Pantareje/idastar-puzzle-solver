module IdaStarSpec (spec) where

import IdaStar (idaStar)
import Test.Hspec

spec :: Spec
spec = do
  describe "idaStar" $ do
    it "El camino más corto en un grafo simple" $ do
      let getNeighbors n = case n of
            0 -> [("a", 1)]
            1 -> [("b", 2), ("a", 0)]
            2 -> [("c", 3), ("b", 1)]
            3 -> [("c", 2)]
            _ -> []
          cost = const 1
          heuristic n = abs (3 - n)
      idaStar getNeighbors cost heuristic 3 0 `shouldBe` Just ["a", "b", "c"]

    it "El camino óptimo en un grafo con ramificaciones y ramas muertas" $ do
      let getNeighbors n = case n of
            0 -> [("a", 1)]
            1 -> [("a", 0), ("b", 3)]
            2 -> [("c", 0), ("d", 3)]
            3 -> [("b", 1), ("d", 2), ("e", 4)]
            4 -> [("e", 3)]
            _ -> []
          cost = const 1
          heuristic n = abs (4 - n)
      idaStar getNeighbors cost heuristic 4 0 `shouldBe` Just ["a", "b", "e"]

    it "Devuelve `Nothing` si no hay solución" $ do
      let getNeighbors 0 = [((), 1)]
          getNeighbors 1 = [((), 0)]
          getNeighbors _ = []
          cost = const 1
          heuristic n = abs (1 - n)
      idaStar getNeighbors cost heuristic 2 0 `shouldBe` Nothing

    it "Devuelve un camino vacío si el origen es igual al destino" $ do
      let getNeighbors 0 = [((), 1)]
          getNeighbors 1 = [((), 0)]
          getNeighbors _ = []
          cost = const 1
          heuristic n = abs (1 - n)
      idaStar getNeighbors cost heuristic 0 0 `shouldBe` Just []
