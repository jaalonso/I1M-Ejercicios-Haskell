-- Programacion_dinamica_Turista_en_Manhattan.hs
-- Programación dinámica: Turista en Manhattan
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Programacion_dinamica_Turista_en_Manhattan where

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- En el siguiente gráfico se representa en una cuadrícula el plano de
-- Manhattan. Cada línea es una opción a seguir; el número representa
-- las atracciones que se pueden visitar si se elige esa opción.
--
--          3         2         4         0
--     * ------- * ------- * ------- * ------- *
--     |         |         |         |         |
--     |1        |0        |2        |4        |3
--     |    3    |    2    |    4    |    2    |
--     * ------- * ------- * ------- * ------- *
--     |         |         |         |         |
--     |4        |6        |5        |2        |1
--     |    0    |    7    |    3    |    4    |
--     * ------- * ------- * ------- * ------- *
--     |         |         |         |         |
--     |4        |4        |5        |2        |1
--     |    3    |    3    |    0    |    2    |
--     * ------- * ------- * ------- * ------- *
--     |         |         |         |         |
--     |5        |6        |8        |5        |3
--     |    1    |    3    |    2    |    2    |
--     * ------- * ------- * ------- * ------- *
--
-- El turista entra por el extremo superior izquierda y sale por el
-- extremo inferior derecha. Sólo puede moverse en las direcciones Sur y
-- Este (es decir, hacia abajo o hacia la derecha).
--
-- Representamos el mapa mediante una matriz p tal que p(i,j) = (a,b),
-- donde a = nº de atracciones si se va hacia el sur y b = nº de
-- atracciones si se va al este. Además, ponemos un 0 en el valor del
-- número de atracciones por un camino que no se puede elegir. De esta
-- forma, el mapa anterior se representa por la matriz siguiente:
--
--    ( (1,3)   (0,2)   (2,4)   (4,0)  (3,0) )
--    ( (4,3)   (6,2)   (5,4)   (2,2)  (1,0) )
--    ( (4,0)   (4,7)   (5,3)   (2,4)  (1,0) )
--    ( (5,3)   (6,3)   (8,0)   (5,2)  (3,0) )
--    ( (0,1)   (0,3)   (0,2)   (0,2)  (0,0) )
--
-- En este caso, si se hace el recorrido
--    [S, E, S, E, S, S, E, E],
-- el número de atracciones es
--     1  3  6  7  5  8  2  2
-- cuya suma es 34.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Matrix

-- ---------------------------------------------------------------------
-- § Ejercicios                                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por recursión, la función
--    mayorNumeroVR :: Matrix (Int,Int) -> Int
-- tal que (mayorNumeroVR p) es el máximo número de atracciones que se
-- pueden visitar en el plano representado por la matriz p. Por ejemplo,
-- si se define la matriz anterior por
--    ej1, ej2, ej3 :: Matrix (Int,Int)
--    ej1 = fromLists [[(1,3),(0,2),(2,4),(4,0),(3,0)],
--                     [(4,3),(6,2),(5,4),(2,2),(1,0)],
--                     [(4,0),(4,7),(5,3),(2,4),(1,0)],
--                     [(5,3),(6,3),(8,0),(5,2),(3,0)],
--                     [(0,1),(0,3),(0,2),(0,2),(0,0)]]
--    ej2 = fromLists [[(1,3),(0,0)],
--                     [(0,3),(0,0)]]
--    ej3 = fromLists [[(1,3),(0,2),(2,0)],
--                     [(4,3),(6,2),(5,0)],
--                     [(0,0),(0,7),(0,0)]]
-- entonces
--    mayorNumeroVR ej1 == 34
--    mayorNumeroVR ej2 == 4
--    mayorNumeroVR ej3 == 17
-- ---------------------------------------------------------------------

ej1, ej2, ej3 :: Matrix (Int,Int)
ej1 = fromLists [[(1,3),(0,2),(2,4),(4,0),(3,0)],
                 [(4,3),(6,2),(5,4),(2,2),(1,0)],
                 [(4,0),(4,7),(5,3),(2,4),(1,0)],
                 [(5,3),(6,3),(8,0),(5,2),(3,0)],
                 [(0,1),(0,3),(0,2),(0,2),(0,0)]]
ej2 = fromLists [[(1,3),(0,0)],
                 [(0,3),(0,0)]]
ej3 = fromLists [[(1,3),(0,2),(2,0)],
                 [(4,3),(6,2),(5,0)],
                 [(0,0),(0,7),(0,0)]]

mayorNumeroVR :: Matrix (Int,Int) -> Int
mayorNumeroVR p = aux m n
  where m = nrows p
        n = ncols p
        aux 1 1 = 0
        aux 1 j = sum [snd (p !(1,k)) | k <-[1..j-1]]
        aux i 1 = sum [fst (p !(k,1)) | k <-[1..i-1]]
        aux i j = max (aux (i-1) j + fst (p !(i-1,j)))
                      (aux i (j-1) + snd (p !(i,j-1)))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, por programación dinámica, la función
--    mayorNumeroVPD :: Matrix (Int,Int) -> Int
-- tal que (mayorNumeroVPD p) es el máximo número de atracciones que se
-- pueden visitar en el plano representado por la matriz p. Por ejemplo,
--    mayorNumeroVPD ej1 == 34
--    mayorNumeroVPD ej2 == 4
--    mayorNumeroVPD ej3 == 17
-- ---------------------------------------------------------------------

mayorNumeroVPD :: Matrix (Int,Int) -> Int
mayorNumeroVPD p = matrizNumeroV p ! (m,n)
  where m = nrows p
        n = ncols p

matrizNumeroV :: Matrix (Int,Int) -> Matrix Int
matrizNumeroV p = q
  where m = nrows p
        n = ncols p
        q = matrix m n f
          where f (1,1) = 0
                f (1,j) = sum [snd (p !(1,k)) | k <-[1..j-1]]
                f (i,1) = sum [fst (p !(k,1)) | k <-[1..i-1]]
                f (i,j) = max (q !(i-1,j) + fst (p !(i-1,j)))
                              (q !(i,j-1) + snd (p !(i,j-1)))

-- ---------------------------------------------------------------------
-- Ejercicio 3. Comparar la eficiencia observando las estadísticas
-- correspondientes a los siguientes cálculos
--    mayorNumeroVR  (fromList 13 13 [(n,n+1) | n <- [1..]])
--    mayorNumeroVPD (fromList 13 13 [(n,n+1) | n <- [1..]])
-- ---------------------------------------------------------------------

-- La comparación es
--    λ> mayorNumeroVR (fromList 13 13 [(n,n+1) | n <- [1..]])
--    2832
--    (6.54 secs, 5,179,120,504 bytes)
--    λ> mayorNumeroVPD (fromList 13 13 [(n,n+1) | n <- [1..]])
--    2832
--    (0.01 secs, 670,128 bytes)
