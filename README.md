# Solucionador de Rompecabezas con IDA*

## Descripción

Este proyecto implementa un solucionador de rompecabezas deslizantes utilizando el algoritmo [IDA* (Iterative Deepening A*)](https://es.wikipedia.org/wiki/IDA*). El programa permite resolver rompecabezas de cualquier tamaño cuadrado, encontrando la secuencia óptima de movimientos para llegar a la solución. Calcula rápidamente soluciones para rompecabezas con hasta 15 casillas y un hueco, con un total de 10,46 billones de posiciones posibles aproximadamente.

## Características

- Implementa el algoritmo IDA* con la distancia de las casillas a su posición como heurística.
- Permite definir el tamaño del rompecabezas.
- Verifica con precisión si un rompecabezas tiene solución antes de intentar resolverlo.
- Visualiza paso a paso la resolución del rompecabezas.
- Depende únicamente de la biblioteca base de Haskell.

## Requisitos

- GHC (Glasgow Haskell Compiler).
- Cabal (sistema de construcción y gestión de paquetes para Haskell).

Al ser un proyecto sencillo, también sería posible compilar los ficheros del código fuente a mano si no se usase Cabal, aunque no es recomendable.

## Compilación

Para compilar el proyecto, simplemente basta con ejecutar `cabal`:

```
cabal build
```

## Uso

Para ejecutar el programa:

```
cabal run
```

El programa te pedirá:

1. El tamaño del lado del rompecabezas (por ejemplo, 4 para un rompecabezas 4x4).
2. El estado inicial del rompecabezas como una lista de números separados por espacios, donde 0 representa el espacio vacío.

Ejemplo de entrada para un rompecabezas 4x4:

```
Introduce el tamaño del lado:
4
Introduce el estado inicial:
1 5 2 7 10 14 11 6 15 12 9 3 13 0 8 4
```

Si el rompecabezas tiene solución, el programa mostrará:
- La secuencia de movimientos necesarios para resolverlo.
- La opción de visualizar la solución paso a paso.

## Sobre el Algoritmo IDA*

IDA* (_Iterative Deepening A*_) es una variante del algoritmo [IDDFS](https://es.wikipedia.org/wiki/Búsqueda_en_profundidad_iterativa) que utiliza mucha menos memoria, ya que no memoriza todos los nodos visitados. Funciona realizando una serie de búsquedas en profundidad con un límite de coste que va aumentando progresivamente.

Los puntos fuertes de IDA* son:
- Un consumo de memoria muy reducido, estando acotado por la profundidad y no por su anchura.
- Garantiza encontrar siempre el camino óptimo, siempre y cuando la heurística sea admisible.

Sin embargo, el algoritmo recorre continuamente los mismos nodos en cada nueva iteración, reiniciando la búsqueda desde el nodo inicial con un umbral de coste mayor en cada ciclo.

Este proyecto utiliza la distancia rectilínea de todas las casillas a su posición como heurística admisible. Además, modifica el algoritmo IDA* para no comprobar la lista completa de todos los nodos ya visitados, únicamente evitando volver a la posición anterior. Eliminar la comprobación de los nodos visitados en la cola redujo significativamente el tiempo hasta encontrar la solución, independientemente de la cantidad de movimientos requeridos.

## Licencia

Este proyecto está licenciado bajo la Licencia Pública de la Unión Europea (EUPL) versión 1.2 - consulta el archivo [LICENSE](LICENSE) para más detalles.
