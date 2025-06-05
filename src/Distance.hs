-- |
-- Module      : Distance
-- Description : Funciones para calcular distancias entre puntos
-- Copyright   : © 2025 Sergio Gil
-- License     : EUPL-1.2
--
-- Proporciona funciones para calcular diferentes tipos de distancias
-- entre puntos en un espacio.
module Distance (rectilinear) where

-- |
-- Calcula la distancia rectilínea entre dos puntos.
--
-- La distancia rectilínea entre dos puntos es la suma de las diferencias absolutas
-- de sus coordenadas. Para dos puntos (r1, c1) y (r2, c2), la distancia rectilínea
-- es |r1 - r2| + |c1 - c2|.
--
-- @
-- rectilinear (0, 0) (3, 4) = 7  -- 3 pasos horizontales + 4 verticales
-- rectilinear (1, 2) (1, 5) = 3  -- 0 pasos horizontales + 3 verticales
-- @
rectilinear :: (Int, Int) -> (Int, Int) -> Int
rectilinear (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)
