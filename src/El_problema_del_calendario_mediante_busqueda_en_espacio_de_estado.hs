-- El_problema_del_calendario_mediante_busqueda_en_espacio_de_estado.hs
-- El problema del calendario mediante búsqueda en espacio de estado.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module El_problema_del_calendario_mediante_busqueda_en_espacio_de_estado where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El problema del calendario, para una competición deportiva en la que
-- se enfrentan n participantes, consiste en elaborar un calendario de
-- forma que:
--    + el campeonato dure n-1 días,
--    + cada participante juegue exactamente un partido diario y
--    + cada participante juegue exactamente una vez con cada adversario.
-- Por ejemplo, con 8 participantes una posible solución es
--      | 1 2 3 4 5 6 7
--    --+--------------
--    1 | 2 3 4 5 6 7 8
--    2 | 1 4 3 6 5 8 7
--    3 | 4 1 2 7 8 5 6
--    4 | 3 2 1 8 7 6 5
--    5 | 6 7 8 1 2 3 4
--    6 | 5 8 7 2 1 4 3
--    7 | 8 5 6 3 4 1 2
--    8 | 7 6 5 4 3 2 1
-- donde las filas indican los jugadores y las columnas los días; es
-- decir, el elemento (i,j) indica el adversario del jugador i el día j;
-- por ejemplo, el adversario del jugador 2 el 4ª día es el jugador 6.
--
-- El objetivo de esta relación de ejercicios es resolver el problema
-- del calendario mediante búsqueda en espacio de estados, utilizando las
-- implementaciones estudiadas en el tema 23
--    https://jaalonso.github.io/cursos/i1m/temas/tema-23.html
--
-- Para realizar los ejercicios hay que tener instalada la librería de
-- I1M. Para instalarla basta ejecutar en una consola
--    cabal update
--    cabal install I1M

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import I1M.BusquedaEnEspaciosDeEstados
import Data.Matrix
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir el tipo Calendario como una matriz de números
-- enteros.
-- ---------------------------------------------------------------------

type Calendario = Matrix Int

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    inicial :: Int -> Calendario
-- tal que (inicial n) es el estado inicial para el problema del
-- calendario con n participantes; es decir, una matriz de n fila y n-1
-- columnas con todos sus elementos iguales a 0. Por ejemplo,
--    λ> inicial 4
--    ( 0 0 0 )
--    ( 0 0 0 )
--    ( 0 0 0 )
--    ( 0 0 0 )
-- ---------------------------------------------------------------------

inicial :: Int -> Calendario
inicial n = zero n (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    sucesores :: Int -> Calendario -> [Calendario]
-- tal que (sucesores n c) es la lista de calendarios, para el problema
-- con n participantes, obtenidos poniendo en el lugar del primer
-- elemento nulo de c uno de los posibles jugadores de forma que se
-- cumplan las condiciones del problema. Por ejemplo,
--    λ> sucesores 4 (fromLists [[2,3,0],[1,0,0],[0,1,0],[0,0,0]])
--    [( 2 3 4 )
--     ( 1 0 0 )
--     ( 0 1 0 )
--     ( 0 0 1 )]
--    λ> sucesores 4 (fromLists [[2,3,4],[1,0,0],[0,1,0],[0,0,1]])
--    [( 2 3 4 )
--     ( 1 4 0 )
--     ( 0 1 0 )
--     ( 0 2 1 )]
-- ---------------------------------------------------------------------

sucesores :: Int -> Calendario -> [Calendario]
sucesores n c =
  [setElem i (k,j) (setElem k (i,j) c) |
   k <- [1..n] \\ (i : [c!(k,j) | k <- [1..i-1]] ++
                       [c!(i,k) | k <- [1..j-1]]),
   c!(k,j) == 0]
  where (i,j) = head [(a,b) | a <- [1..n], b <- [1..n-1], c!(a,b) == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    esFinal :: Int -> Calendario -> Bool
-- tal que (final n c) se verifica si c un estado final para el problema
-- del calendario con n participantes; es decir, no queda en c ningún
-- elemento igual a 0. Por ejemplo,
--    λ> esFinal 4 (fromLists [[2,3,4],[1,4,3],[4,1,2],[3,2,1]])
--    True
--    λ> esFinal 4 (fromLists [[2,3,4],[1,4,3],[4,1,2],[3,2,0]])
--    False
-- ---------------------------------------------------------------------

esFinal :: Int -> Calendario -> Bool
esFinal n c = null [(i,j) | i <- [1..n], j <- [1..n-1], c!(i,j) == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    calendario :: Int -> [Calendario]
-- tal que (calendario n) son las soluciones del problema del calendario,
-- con n participantes, mediante el patrón de búsqueda en espacio de
-- estados. Por ejemplo,
--    λ> head (calendario 6)
--    ( 2 3 4 5 6 )
--    ( 1 4 5 6 3 )
--    ( 5 1 6 4 2 )
--    ( 6 2 1 3 5 )
--    ( 3 6 2 1 4 )
--    ( 4 5 3 2 1 )
--
--    λ> length (calendario 6)
--    720
--    λ> length (calendario 5)
--    0
-- ---------------------------------------------------------------------

calendario :: Int -> [Calendario]
calendario n = buscaEE (sucesores n)
                       (esFinal n)
                       (inicial n)
