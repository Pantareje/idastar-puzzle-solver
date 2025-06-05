-- |
-- Module      : Puzzle
-- Description : Implementación de un rompecabezas.
-- Copyright   : © 2025 Sergio Gil
-- License     : EUPL-1.2
--
-- Funciones para manipular y resolver rompecabezas.
module Puzzle (toPuzzle, fromPuzzle, formatPuzzle, solvePuzzle, tryMove, Puzzle, Move (..)) where

import Control.Exception (assert)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Distance (rectilinear)
import IdaStar (idaStar)

-- |
-- Representa un puzzle deslizante. Es un tablero deslizante de lado `side`, las
-- casillas se representan desde el 0 hasta el número total de casillas, donde 0
-- representa un hueco a donde deslizar las piezas colindantes.
data Puzzle = Puzzle
  { -- | Longitud del lado del tablero cuadrado.
    side :: Int,
    -- | Estado del tablero como una lista plana de enteros.
    board :: [Int],
    -- | Índice de la casilla vacía en el tablero.
    emptyIndex :: Int
  }
  deriving (Show)

instance Eq Puzzle where
  (==) (Puzzle _ b1 _) (Puzzle _ b2 _) = b1 == b2

-- |
-- Crea una cadena con la representación cuadrada
-- de las casillas del rompecabezas.
formatPuzzle :: Puzzle -> [Char]
formatPuzzle (Puzzle n b _) = loop b 0 ""
  where
    loop [] _ acc = acc
    loop (x : xs) count acc
      | count == n - 1 = loop xs 0 (acc ++ pad x ++ "\n")
      | otherwise = loop xs (count + 1) (acc ++ pad x ++ " ")
    pad x =
      let s = show x
       in if length s == 1 then ' ' : s else s

-- |
-- Representa los posibles movimientos en un puzzle deslizante.
--
-- Cada movimiento indica la dirección en la que se mueve **la casilla vacía**.
-- Por ejemplo, `MoveUp` significa que la casilla vacía se mueve hacia arriba,
-- lo que equivale a deslizar la ficha de arriba hacia abajo.
data Move = MoveUp | MoveDown | MoveLeft | MoveRight
  deriving (Eq, Show)

-- |
-- Crea un objetivo con el tamaño de lado especificado. La casilla
-- vacía es la inferior derecha.
solveTarget :: Int -> Puzzle
solveTarget s =
  let len = s * s
   in Puzzle {side = s, board = [1 .. len - 1] ++ [0], emptyIndex = len - 1}

-- |
-- Convierte un tamaño de lado y una lista en un rompecabezas.
toPuzzle :: Int -> [Int] -> Puzzle
toPuzzle s list =
  let n = s * s
   in assert
        (length list == n)
        Puzzle {side = s, board = list, emptyIndex = fromJust (elemIndex 0 list)}

-- |
-- Convierte un rompecabezas en una lista y el tamaño de su lado.
fromPuzzle :: Puzzle -> (Int, [Int])
fromPuzzle p = (side p, board p)

-- |
-- Intenta realizar un movimiento sobre el rompecabezas. Devuelve `Just` con
-- el rompecabezas movido si tiene éxito o `Nothing` en caso contrario.
tryMove :: Puzzle -> Move -> Maybe Puzzle
tryMove p m
  -- Comprobamos que el movimiento se encuentre dentro de los límites.
  | ex >= 0 && ex < n && ey >= 0 && ey < n = Just (swapZero p (ex, ey))
  | otherwise = Nothing
  where
    n = side p
    (dx, dy) = case m of
      MoveUp -> (-1, 0)
      MoveDown -> (1, 0)
      MoveLeft -> (0, -1)
      MoveRight -> (0, 1)
    (sx, sy) = toPos n (emptyIndex p)
    (ex, ey) = (sx + dx, sy + dy)

-- |
-- Devuelve `True` si el rompecabezas se puede
-- resolver y `False` en caso contrario.
solveable :: Puzzle -> Bool
solveable p
  | odd n = even inversions
  | otherwise = odd (inversions + r)
  where
    b = board p
    inversions =
      length
        [ (i, j)
        | i <- [0 .. length b - 1],
          j <- [0 .. length b - 1],
          i < j,
          b !! i /= 0,
          b !! j /= 0,
          b !! i > b !! j
        ]
    n = side p
    r = n - (emptyIndex p `div` n)

-- |
-- Intenta resolver el rompecabezas, devolviendo `Just` con
-- la lista de movimientos tomados si es exitoso y `Nothing`
-- si no encuentra la solución.
solvePuzzle :: Puzzle -> Maybe [Move]
solvePuzzle p
  | solveable p = idaStar getMoves (const 1) heuristic (solveTarget (side p)) p
  | otherwise = Nothing

-- |
-- Obtiene las coordenadas bidimensionales dado el índice de casilla.
toPos :: Int -> Int -> (Int, Int)
toPos s i = (i `div` s, i `mod` s)

-- |
-- Obtiene el índice de casilla dadas las coordenadas bidimensionales.
fromPos :: Int -> (Int, Int) -> Int
fromPos s (r, c) = r * s + c

-- |
-- Intercambia el espacio en blanco con otra casilla del rompecabezas.
swapZero :: Puzzle -> (Int, Int) -> Puzzle
swapZero p (re, ce) =
  Puzzle
    { side = n,
      board = [swap i v | (i, v) <- zip [0 ..] (board p)],
      emptyIndex = e
    }
  where
    n = side p
    b = board p
    s = emptyIndex p
    e = fromPos (side p) (re, ce)
    swap index val
      | index == s = b !! e
      | index == e = b !! s
      | otherwise = val

-- |
-- Obtiene los rompecabezas alcanzables con un movimiento desde
-- el rompecabezas actual junto a qué movimiento usar para alcanzar
-- cada rompecabezas.
getMoves :: Puzzle -> [(Move, Puzzle)]
getMoves p =
  [ (m, p')
  | m <- [MoveUp, MoveDown, MoveLeft, MoveRight],
    let maybeP' = tryMove p m,
    maybeP' /= Nothing,
    let p' = fromJust maybeP'
  ]

-- |
-- La heurística usada es la suma de la distancia rectilínea de
-- cada casilla (excepto la vacía) a su posición objetivo.
heuristic :: Puzzle -> Int
heuristic p =
  let n = side p
   in sum [rectilinear (toPos n i) (toPos n (v - 1)) | (i, v) <- zip [0 .. 15] (board p), v /= 0]
