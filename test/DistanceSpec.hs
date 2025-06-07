module DistanceSpec (spec) where

import Distance (rectilinear)
import Test.Hspec

spec :: Spec
spec = do
  describe "rectilinear" $ do
    it "Calcula la distancia rectilínea entre dos puntos" $ do
      rectilinear (0, 0) (3, 4) `shouldBe` 7
      rectilinear (1, 2) (1, 5) `shouldBe` 3
      rectilinear (5, 5) (2, 3) `shouldBe` 5

    it "Es cero si y sólo si los puntos son iguales" $ do
      rectilinear (0, 0) (0, 0) `shouldBe` 0
      rectilinear (5, 7) (5, 7) `shouldBe` 0
      rectilinear (1, 2) (3, 4) `shouldSatisfy` (> 0)

    it "Nunca es negativa" $ do
      rectilinear (0, 0) (3, 4) `shouldSatisfy` (>= 0)
      rectilinear (1, 2) (1, 5) `shouldSatisfy` (>= 0)
      rectilinear (5, 5) (2, 3) `shouldSatisfy` (>= 0)
      rectilinear (-3, -4) (0, 0) `shouldSatisfy` (>= 0)

    it "Es simétrica" $ do
      rectilinear (0, 0) (3, 4) `shouldBe` rectilinear (3, 4) (0, 0)
      rectilinear (1, 2) (5, 6) `shouldBe` rectilinear (5, 6) (1, 2)

    it "Satisface la desigualdad triangular" $ do
      let a = (0, 0)
          b = (3, 0)
          c = (3, 4)
      rectilinear a c `shouldSatisfy` (<= rectilinear a b + rectilinear b c)
      rectilinear b a `shouldSatisfy` (<= rectilinear b c + rectilinear c a)
