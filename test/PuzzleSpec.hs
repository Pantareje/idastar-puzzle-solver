{-# LANGUAGE LambdaCase #-}

module PuzzleSpec (spec) where

import Puzzle
import Test.Hspec

spec :: Spec
spec = do
  describe "toPuzzle y fromPuzzle" $ do
    it "Convierte entre la representación de lista y `Puzzle`" $ do
      let size = 3
          board = [1, 2, 3, 4, 5, 6, 7, 8, 0]
          puzzle = toPuzzle size board
          (size', board') = fromPuzzle puzzle

      size' `shouldBe` size
      board' `shouldBe` board

  describe "formatPuzzle" $ do
    it "Formatea correctamente un rompecabezas" $ do
      let puzzle = toPuzzle 3 [1, 2, 3, 4, 5, 6, 7, 8, 0]
          expected = " 1  2  3\n 4  5  6\n 7  8  0\n"

      formatPuzzle puzzle `shouldBe` expected

  describe "tryMove" $ do
    let bottomRight = toPuzzle 3 [1, 2, 3, 4, 5, 6, 7, 8, 0]
    let topLeft = toPuzzle 3 [0, 1, 2, 3, 4, 5, 6, 7, 8]

    it "Devuelve `Nothing` para movimientos inválidos" $ do
      tryMove bottomRight MoveDown `shouldBe` Nothing
      tryMove bottomRight MoveRight `shouldBe` Nothing
      tryMove topLeft MoveUp `shouldBe` Nothing
      tryMove topLeft MoveLeft `shouldBe` Nothing

    it "Mueve el espacio vacío correctamente" $ do
      let upResult = tryMove bottomRight MoveUp
      let leftResult = tryMove bottomRight MoveLeft

      upResult `shouldSatisfy` (/= Nothing)
      leftResult `shouldSatisfy` (/= Nothing)

      case upResult of
        Just p -> fromPuzzle p `shouldBe` (3, [1, 2, 3, 4, 5, 0, 7, 8, 6])
        Nothing -> expectationFailure "MoveUp debería ser válido"

      case leftResult of
        Just p -> fromPuzzle p `shouldBe` (3, [1, 2, 3, 4, 5, 6, 7, 0, 8])
        Nothing -> expectationFailure "MoveLeft debería ser válido"

  describe "solvePuzzle" $ do
    it "Devuelve `Nothing` para rompecabezass irresolubles" $ do
      let unsolvable = toPuzzle 3 [2, 1, 3, 4, 5, 6, 7, 8, 0]
      solvePuzzle unsolvable `shouldBe` Nothing

    it "Devuelve una secuencia vacía si el rompecabezas ya está resuelto" $ do
      let solved = toPuzzle 3 [1, 2, 3, 4, 5, 6, 7, 8, 0]
      solvePuzzle solved `shouldBe` Just []

    it "Resuelve un rompecabezas de tamaño enorme a un movimiento de la solución" $ do
      let almostSolved5 = toPuzzle 5 ([1 .. 23] ++ [0, 24])
      case solvePuzzle almostSolved5 of
        Just moves -> length moves `shouldBe` 1
        Nothing -> expectationFailure "El rompecabezas debería resolverse en un movimiento"

    it "Resuelve un rompecabezas sencillo" $ do
      let simple = toPuzzle 3 [1, 8, 2, 4, 0, 3, 7, 6, 5]
      case solvePuzzle simple of
        Just moves -> length moves `shouldBe` 8
        Nothing -> expectationFailure "El rompecabezas debería resolverse en 8 movimientos"

    describe "rompecabezass complejos" $ do
      it "Resuelve correctamente un rompecabezas complejo (34 movimientos)" $ do
        let p34 = toPuzzle 4 [1, 5, 2, 7, 10, 14, 11, 6, 15, 12, 9, 3, 13, 0, 8, 4]
        case solvePuzzle p34 of
          Just moves -> length moves `shouldBe` 34
          Nothing -> expectationFailure "El rompecabezas debería resolverse (34 movimientos)"

      it "Resuelve correctamente un rompecabezas complejo (38 movimientos)" $ do
        let p38 = toPuzzle 4 [5, 6, 10, 7, 1, 3, 11, 8, 13, 4, 15, 9, 14, 0, 2, 12]
        case solvePuzzle p38 of
          Just moves -> length moves `shouldBe` 38
          Nothing -> expectationFailure "El rompecabezas debería resolverse (38 movimientos)"

      it "Resuelve correctamente un rompecabezas complejo (40 movimientos)" $ do
        let p40 = toPuzzle 4 [1, 11, 6, 2, 10, 13, 15, 5, 3, 12, 0, 4, 9, 7, 14, 8]
        case solvePuzzle p40 of
          Just moves -> length moves `shouldBe` 40
          Nothing -> expectationFailure "El rompecabezas debería resolverse (40 movimientos)"

      it "Resuelve correctamente un rompecabezas complejo (52 movimientos)" $ do
        let p52 = toPuzzle 4 [5, 8, 13, 15, 14, 0, 1, 7, 4, 6, 10, 2, 11, 9, 12, 3]
        case solvePuzzle p52 of
          Just moves -> length moves `shouldBe` 52
          Nothing -> expectationFailure "El rompecabezas debería resolverse (52 movimientos)"

      it "Resuelve correctamente un rompecabezas complejo (54 movimientos)" $ do
        let p54 = toPuzzle 4 [6, 8, 12, 13, 7, 2, 5, 14, 9, 3, 1, 15, 11, 0, 10, 4]
        case solvePuzzle p54 of
          Just moves -> length moves `shouldBe` 54
          Nothing -> expectationFailure "El rompecabezas debería resolverse (54 movimientos)"

      it "Resuelve correctamente un rompecabezas complejo (56 movimientos)" $ do
        let p56 = toPuzzle 4 [9, 3, 12, 5, 4, 14, 6, 11, 8, 7, 15, 13, 10, 0, 2, 1]
        case solvePuzzle p56 of
          Just moves -> length moves `shouldBe` 56
          Nothing -> expectationFailure "El rompecabezas debería resolverse (56 movimientos)"
