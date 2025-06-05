-- |
-- Module      : IdaStar
-- Description : Implementación del algoritmo de búsqueda IDA*
-- Copyright   : © 2025 Sergio Gil
-- License     : EUPL-1.2
--
-- Implementa el algoritmo de búsqueda IDA*
-- para encontrar caminos en grafos.
module IdaStar (idaStar) where

{-
 Dado un grafo dirigido `G = (V, E)`, buscamos un camino mínimo desde
 un nodo inicial `s` a un nodo objetivo `t`. Para cada nodo de `V`,
 definimos:

 - `g(n)`: el coste acumulado del camino desde `s` hasta `n`.
 - `h(n)`: la función heurística admisible para estimar el coste.
 - `f(n) = g(n) + h(n)`: estimación del coste total del camino.

 Para `s`, tomamos `g(s) = 0`. Por tanto, `f(s) = h(s)`.

 Inicialmente, elegimos como umbral `u <- f(s)`, que es `u <- h(s)`.
 Realizamos una búsqueda en profundidad, expandiendo nodos únicamente
 si `f(n) <= u`. Si hallamos el camino mínimo hasta el objetivo, este
 es el resultado final. En caso contrario, actualizamos el umbral:

 `u' = min { f(n) | f(n) > u, con n generado en la búsqueda }`.

 Asignamos `u <- u'` y repetimos la búsqueda con el nuebo umbral.
-}

-- |
-- Encuentra el camino de menor coste desde un nodo inicial hasta uno
-- objetivo, utilizando una heurística admisible para acotar el
-- espacio de búsqueda. Si la heurística no es admisible, no se garantiza
-- que devuelva el camino más corto.
--
-- Reduce drásticamente el consumo de memoria frente a A*. Sin embargo, tiene
-- que explorar múltiples veces algunos nodos por cada iteración.
--
-- No es seguro usarlo con grafos que puedan tener ciclos sin coste, solamente
-- comprueba no volver al nodo anterior tras cada acción.
idaStar ::
  (Eq n) =>
  -- | getNeighbours: función que devuelve los vecinos de un nodo y la acción tomada
  (n -> [(m, n)]) ->
  -- | w: función que asigna un coste a cada acción entre dos nodos
  (m -> Int) ->
  -- | h: heurística admisible que estima el coste desde un nodo
  (n -> Int) ->
  -- | target: nodo objetivo
  n ->
  -- | start: nodo inicial
  n ->
  -- | lista de acciones desde el nodo inicial al objetivo o `Nothing` si no hay camino
  Maybe [m]
idaStar getNeighbours w h target start = iteration (h start)
  where
    -- Incrementamos el umbral, mientras podamos,
    -- al menor de los umbrales no explorados
    iteration t =
      case search start start 0 t of
        Left path -> Just path
        Right bound
          | bound > t && bound /= maxBound -> iteration bound
          | otherwise -> Nothing

    -- Buscamos el objetivo desde el nodo actual.
    search currentNode mPrevNode g t
      | currentNode == target = Left [] -- Hemos llegado al objetivo, devolvemos camino vacío
      | fVal > t = Right fVal -- f(n) > umbral, devolvemos f(n) para actualizar el umbral
      | otherwise = foldr processMove (Right maxBound) moves -- Exploramos los movimientos posibles
      where
        fVal = g + h currentNode
        -- Filtramos para no volver al nodo inmediatamente anterior
        moves = filter ((/= mPrevNode) . snd) (getNeighbours currentNode)

        -- Procesamos todos los movimientos posibles, devolviendo el camino
        -- acumulado al objetivo o el menor de los umbrales no explorados
        processMove move acc =
          case search (snd move) currentNode (g + w (fst move)) t of
            Left path -> Left (fst move : path) -- Encontramos un camino, añadimos este movimiento
            Right bound ->
              case acc of
                Left _ -> acc -- Ya tenemos un camino, lo mantenemos
                Right b -> Right (min b bound) -- Actualizamos el umbral mínimo
