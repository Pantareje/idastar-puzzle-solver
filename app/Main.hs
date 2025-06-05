-- |
-- Module      : Main
-- Description : Interfaz para interactuar con la aplicación.
-- Copyright   : © 2025 Sergio Gil
-- License     : EUPL-1.2
--
-- Proporciona una interfaz para poder resolver rompecabezas
-- y mostrar una animación de la resolución de estos.
module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.Char (toUpper)
import Data.List (nub)
import Puzzle (Move (..), Puzzle, formatPuzzle, solvePuzzle, toPuzzle, tryMove)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import System.IO.Error (isEOFError)

-- Ejemplos tomados de https://doi.org/10.3897/jucs.65202

-- 1 5 2 7 10 14 11 6 15 12 9 3 13 0 8 4     34 movimientos
-- 5 6 10 7 1 3 11 8 13 4 15 9 14 0 2 12     38 movimientos
-- 1 11 6 2 10 13 15 5 3 12 0 4 9 7 14 8     40 movimientos
-- 5 8 13 15 14 0 1 7 4 6 10 2 11 9 12 3     52 movimientos
-- 6 8 12 13 7 2 5 14 9 3 1 15 11 0 10 4     54 movimientos
-- 9 3 12 5 4 14 6 11 8 7 15 13 10 0 2 1     56 movimientos

main :: IO ()
main = do
  -- Leemos la longitud y los valores, convirtiendo a `Int` y `[Int]`.

  putStrLn "Introduce el tamaño del lado:"
  sideInput <- getLine `catch` eofHandler
  let side = read sideInput :: Int

  putStrLn "Introduce el estado inicial:"
  numsInput <- getLine `catch` eofHandler
  let nums = map read (words numsInput) :: [Int]

  -- Comprobamos que los valores leídos formen un rompecabezas correcto.

  when (not (validLength side nums)) $ do
    putStrLn ("Debes introducir exactamente " ++ show (side * side) ++ " números.")
    exitFailure

  when (not (validRange side nums)) $ do
    putStrLn ("Todos los números deben estar entre 0 y " ++ show (side * side - 1) ++ ".")
    exitFailure

  when (not (noDuplicates nums)) $ do
    putStrLn "No se puede repetir un número."
    exitFailure

  -- Si tiene solución, resolvemos el rompecabezas.

  let puzzle = toPuzzle side nums
  case solvePuzzle puzzle of
    Just sol -> printSolutionStart puzzle sol
    Nothing -> putStrLn "No existe una solución para la entrada dada."

printSolutionStart :: Puzzle -> [Move] -> IO ()
printSolutionStart puzzle sol = do
  -- Mostramos las acciones tomadas para alcanzar la solicitud.
  putStrLn ""
  putStrLn ("Solución encontrada en " ++ show (length sol) ++ " movimientos")
  putStrLn (showMoveListArrows sol)
  putStrLn ""

  -- Preguntamos al usuario si quiere ver la solución animada.
  -- Si no responde, no animamos la solución.
  putStr "¿Animar movimientos? (s/N): "
  hFlush stdout -- Mostramos en la salida.
  animateInput <- getLine `catch` eofHandler
  if map toUpper animateInput == "S"
    then do
      printSolutionStep puzzle sol
    else return ()

printSolutionStep :: Puzzle -> [Move] -> IO ()
printSolutionStep puzzle (s : sx) = do
  -- Mostramos la posición actual del rompecabezas.
  putStrLn ""
  putStrLn (formatPuzzle puzzle)

  -- Preguntamosal usuario si quiere seguir viendo la animación.
  -- Si no responde, continuamos con la animación.
  putStrLn ("Siguiente movimiento: " ++ showMoveArrow s)
  putStr "¿Continuar? (S/n): "
  hFlush stdout -- Mostramos en la salida.
  animateInput <- getLine `catch` eofHandler
  if map toUpper animateInput /= "N"
    then do
      case tryMove puzzle s of
        Just newPuzzle -> printSolutionStep newPuzzle sx
        Nothing -> putStrLn "¡Este movimiento no se puede hacer!"
    else return ()
printSolutionStep puzzle [] = do
  -- Mostramos la solución e indicamos que está resuelto.
  putStrLn ""
  putStrLn (formatPuzzle puzzle)
  putStrLn "¡Está resuelto!"

eofHandler :: IOException -> IO String
eofHandler e
  | isEOFError e = exitSuccess
  | otherwise = ioError e

validLength :: Int -> [Int] -> Bool
validLength side nums = length nums == side * side

validRange :: Int -> [Int] -> Bool
validRange side nums = all (\x -> x >= 0 && x < side * side) nums

noDuplicates :: [Int] -> Bool
-- Si al eliminar los duplicados (`nub`) la longitud se
-- reduce, entonces había duplicados en la lista.
noDuplicates nums = length (nub nums) == length nums

showMoveArrow :: Move -> [Char]
showMoveArrow m = case m of
  MoveUp -> "🡳"
  MoveDown -> "🡱"
  MoveLeft -> "🡺"
  MoveRight -> "🡸"

-- Los movimientos son respecto al hueco.
-- Si los tomamos según cómo se mueven las
-- piezas, estos son al revés.
showMoveListArrows :: [Move] -> [Char]
showMoveListArrows ml = case ml of
  [] -> []
  m : ms -> " " ++ showMoveArrow m ++ showMoveListArrows ms
