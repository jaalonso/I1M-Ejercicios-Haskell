-- Rompecabeza_del_triomino_mediante_divide_y_venceras.hs
-- Rompecabeza del triominó mediante divide y vencerás.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Rompecabeza_del_triomino_mediante_divide_y_venceras where

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- Un poliominó es una figura geométrica plana formada conectando dos o
-- más cuadrados por alguno de sus lados. Los cuadrados se conectan lado
-- con lado, pero no se pueden conectar ni por sus vértices, ni juntando
-- solo parte de un lado de un cuadrado con parte de un lado de otro. Si
-- unimos dos cuadrados se obtiene un dominó, si se juntan tres
-- cuadrados se construye un triominó.
--
-- Sólo existen dos triominós, el I-triomino (por tener forma de I) y el
-- L-triominó (por su forma de L) como se observa en la siguiente figura
--
--    X
---   X     X
--    X     XX
--
-- El rompecabeza del triominó consiste en cubrir un tablero cuadrado
-- con 2^n filas y 2^n columnas, en el que se ha eliminado una casilla,
-- con L-triominós de formas que cubran todas las casillas excepto la
-- eliminada y los triominós no se solapen.
--
-- La casilla eliminada se representará con -1 y los L-triominós con
-- sucesiones de tres números consecutivos en forma de L. Con esta
-- representación una solución del rompecabeza del triominó con 4 filas
-- y la fila eliminada en la posición (4,4) es
--    (  3  3  2  2 )
--    (  3  1  1  2 )
--    (  4  1  5  5 )
--    (  4  4  5 -1 )
--
-- En esta relación resolveremos el rompecabeza del triominó mediante
-- divide y vencerás, utilizando las implementaciones estudiadas en el
-- tema 23 que se encuentra en
--    https://jaalonso.github.io/cursos/i1m/temas/tema-23.html
--
-- La técnica "divide y vencerás" consta de los siguientes pasos:
-- 1. Dividir el problema en subproblemas menores.
-- 2. Resolver por separado cada uno de los subproblemas; si los
--    subproblemas son complejos, usar la misma técnica recursivamente;
--    si son simples, resolverlos directamente.
-- 3. Combinar todas las soluciones de los subproblemas en una solución
--    simple.
--
-- Con (divideVenceras ind resuelve divide combina pbInicial) se
-- resuelve el problema pbInicial mediante la técnica de divide y
-- vencerás, donde
-- + (ind pb) se verifica si el problema pb es indivisible
-- + (resuelve pb) es la solución del problema indivisible pb
-- + (divide pb) es la lista de subproblemas de pb
-- + (combina pb ss) es la combinación de las soluciones ss de los
--      subproblemas del problema pb.
-- + pbInicial es el problema inicial
--
-- En los distintos apartados de esta relación se irán definiendo las
-- anteriores funciones.
--
-- Para realizar los ejercicios hay que tener instalada la librería de
-- I1M. Para instalarla basta ejecutar en una consola
--    cabal update
--    cabal install I1M

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import I1M.DivideVenceras
import Data.Matrix
import Data.List (delete)

-- ---------------------------------------------------------------------
-- § Tipos                                                            --
-- ---------------------------------------------------------------------

-- Los tableros son matrices de números enteros donde -1 representa el
-- hueco, 0 las posiciones sin rellenar y los números mayores que 0
-- representan los triominós.

type Tablero = Matrix Int

-- Los problemas se representarán mediante pares formados por un número
-- natural mayor que 0 (que indica el número con el que se formará el
-- siguiente triominó que se coloque) y un tablero.

type Problema = (Int,Tablero)

-- Las posiciones son pares de números enteros

type Posicion = (Int,Int)

-- ---------------------------------------------------------------------
-- § Problema inicial                                                 --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    tablero :: Int -> Posicion -> Tablero
-- tal que (tablero n p) es el tablero inicial del problema del triominó
-- en un cuadrado nxn en el que se ha eliminado la casilla de la
-- posición (i,j). Por ejemplo,
--    λ> tablero 4 (3,4)
--    (  0  0  0  0 )
--    (  0  0  0  0 )
--    (  0  0  0 -1 )
--    (  0  0  0  0 )
-- ---------------------------------------------------------------------

tablero :: Int -> Posicion -> Tablero
tablero n (i,j) =
  setElem (-1) (i,j) (zero n n)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    pbInicial :: Int -> Posicion -> Problema
-- tal que (pbInicial n p) es el problema inicial del rompecabeza del
-- triominó en un cuadrado nxn en el que se ha eliminado la casilla de
-- la posición p. Por ejemplo,
--    λ> pbInicial 4 (4,4)
--    (1,(  0  0  0  0 )
--       (  0  0  0  0 )
--       (  0  0  0  0 )
--       (  0  0  0 -1 ))
-- ---------------------------------------------------------------------

pbInicial :: Int -> Posicion -> Problema
pbInicial n p = (1,tablero n p)

-- ---------------------------------------------------------------------
-- § Problemas indivisibles                                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    ind :: Problema -> Bool
-- tal que (ind pb) se verifica si el problema pb es indivisible. Por
-- ejemplo,
--    ind (pbInicial 2 (1,2))  ==  True
--    ind (pbInicial 4 (1,2))  ==  False
-- ---------------------------------------------------------------------

ind :: Problema -> Bool
ind (_,p) = ncols p == 2

-- ---------------------------------------------------------------------
-- § Resolución de problemas indivisibles                             --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    posicionHueco :: Tablero -> Posicion
-- tal que (posicionHueco t) es la posición del hueco en el tablero
-- t. Por ejemplo,
--    posicionHueco (tablero 8 (5,2))  ==  (5,2)
-- ---------------------------------------------------------------------

posicionHueco :: Tablero -> Posicion
posicionHueco p =
  head [(i,j) | i <- [1..nrows p],
                j <- [1..ncols p],
                p!(i,j) /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    cuadranteHueco :: Tablero -> Int
-- tal que (cuadranteHueco p) es el cuadrante donde se encuentra el
-- hueco del tablero t (donde la numeración de los cuadrantes es 1 el
-- superior izquierdo, 2 el inferior izquierdo, 3 el superior derecho y 4
-- el inferior derecho). Por ejemplo,
--    cuadranteHueco (tablero 8 (4,4))  ==  1
--    cuadranteHueco (tablero 8 (5,2))  ==  2
--    cuadranteHueco (tablero 8 (3,6))  ==  3
--    cuadranteHueco (tablero 8 (6,6))  ==  4
-- ---------------------------------------------------------------------

cuadranteHueco :: Tablero -> Int
cuadranteHueco t
  | i <= x && j <= x = 1
  | i >  x && j <= x = 2
  | i <= x && j >  x = 3
  | otherwise        = 4
  where (i,j) = posicionHueco t
        x     = nrows t `div` 2

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    centralHueco :: Tablero -> Posicion
-- tal que (centralHueco t) es la casilla central del cuadrante del
-- tablero t donde se encuentra el hueco. Por ejemplo,
--    centralHueco (tablero 8 (5,2))  ==  (5,4)
--    centralHueco (tablero 8 (4,4))  ==  (4,4)
--    centralHueco (tablero 8 (3,6))  ==  (4,5)
--    centralHueco (tablero 8 (6,6))  ==  (5,5)
-- ---------------------------------------------------------------------

centralHueco :: Tablero -> Posicion
centralHueco t =
  case (cuadranteHueco t) of
    1 -> (x,x)
    2 -> (x+1,x)
    3 -> (x,x+1)
    _ -> (x+1,x+1)
  where x = nrows t `div` 2

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    centralesSinHueco :: Tablero -> [Posicion]
-- (centralesSinHueco t) son las posiciones centrales del tablero t de
-- los cuadrantes sin hueco. Por ejemplo,
--    centralesSinHueco (tablero 8 (5,2))  ==  [(4,4),(4,5),(5,5)]
-- ---------------------------------------------------------------------

centralesSinHueco :: Tablero -> [Posicion]
centralesSinHueco t =
  delete (i,j) [(x,x),(x+1,x),(x,x+1),(x+1,x+1)]
  where x     = nrows t `div` 2
        (i,j) = centralHueco t

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    actualiza :: Matrix a -> [((Int,Int),a)] -> Matrix a
-- tal que (actualiza t ps) es la matriz obtenida cambiando en t los
-- valores del las posiciones indicadas en ps por sus correspondientes
-- valores. Por ejemplo,
--    λ> actualiza (identity 3) [((1,2),4),((3,1),5)]
--    ( 1 4 0 )
--    ( 0 1 0 )
--    ( 5 0 1 )
-- ---------------------------------------------------------------------

actualiza :: Matrix a -> [((Int,Int),a)] -> Matrix a
actualiza p []             = p
actualiza p (((i,j),x):zs) = setElem x (i,j) (actualiza p zs)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    triominoCentral :: Problema -> Tablero
-- tal que (triominoCentral (n,t) es el tablero obtenido colocando el
-- triominó formado por el número n en las posiciones centrales de los 3
-- cuadrantes que no contienen el hueco. Por ejemplo,
--    λ> triominoCentral (7,tablero 4 (4,4))
--    (  0  0  0  0 )
--    (  0  7  7  0 )
--    (  0  7  0  0 )
--    (  0  0  0 -1 )
-- ---------------------------------------------------------------------

triominoCentral :: Problema -> Tablero
triominoCentral (n,t) =
  actualiza t [((i,j),n) | (i,j) <- centralesSinHueco t]

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    resuelve :: Problema -> Tablero
-- tal que (resuelve p) es la solución del problema indivisible p. Por
-- ejemplo,
--    λ> tablero 2 (2,2)
--    (  0  0 )
--    (  0 -1 )
--
--    λ> resuelve (5,tablero 2 (2,2))
--    (  5  5 )
--    (  5 -1 )
-- ---------------------------------------------------------------------

resuelve :: Problema -> Tablero
resuelve = triominoCentral

-- ---------------------------------------------------------------------
-- § División en subproblemas                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    divide :: Problema -> [Problema]
-- tal que (divide (n,t)) es la lista de de los problemas obtenidos
-- colocando el triominó n en las casillas centrales de t que no
-- contienen el hueco y dividir el tablero en sus cuatros cuadrantes y
-- aumentar en uno el número del correspondiente triominó. Por ejemplo,
--    λ> divide (3,tablero 4 (4,4))
--    [(4,(  0  0 )
--        (  3  0 )),
--     (5,(  0  0 )
--        (  0  3 )),
--     (6,(  0  3 )
--        (  0  0 )),
--     (7,(  0  0 )
--        (  0 -1 ))]
-- ---------------------------------------------------------------------

divide :: Problema -> [Problema]
divide (n,t) =
  [(n+1, submatrix 1     x (x+1) m q),
   (n+2, submatrix 1     x 1     x q),
   (n+3, submatrix (x+1) m 1     x q),
   (n+4, submatrix (x+1) m (x+1) m q)]
  where q = triominoCentral (n,t)
        m = nrows t
        x = m `div` 2

-- ---------------------------------------------------------------------
-- § Combinación de soluciones                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    combina :: Problema -> [Tablero] -> Tablero
-- tal que (combina p ts) es la combinación de las soluciones ts de los
-- subproblemas del problema p. Por ejemplo,
--    λ> let inicial = (1,tablero 4 (4,4)) :: (Int,Matrix Int)
--    λ> let [p1,p2,p3,p4] = divide inicial
--    λ> let [s1,s2,s3,s4] = map resuelve [p1,p2,p3,p4]
--    λ> combina inicial [s1,s2,s3,s4]
--    (  3  3  2  2 )
--    (  3  1  1  2 )
--    (  4  1  5  5 )
--    (  4  4  5 -1 )
-- ---------------------------------------------------------------------

combina :: Problema -> [Tablero] -> Tablero
combina _ [s1,s2,s3,s4] = joinBlocks (s2,s1,s3,s4)
combina _ _             = error "Imposible"

-- ---------------------------------------------------------------------
-- § Solución mediante divide y vencerás                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    triomino :: Int -> Posicion -> Tablero
-- tal que (triomino n p) es la solución, mediante divide y vencerás,
-- del rompecabeza del triominó en un cuadrado nxn en el que se ha
-- eliminado la casilla de la posición p. Por ejemplo,
--    λ> triomino 4 (4,4)
--    (  3  3  2  2 )
--    (  3  1  1  2 )
--    (  4  1  5  5 )
--    (  4  4  5 -1 )
--
--    λ> triomino 4 (2,3)
--    (  3  3  2  2 )
--    (  3  1 -1  2 )
--    (  4  1  1  5 )
--    (  4  4  5  5 )
--
--    λ> triomino 16 (5,6)
--    (  7  7  6  6  6  6  5  5  6  6  5  5  5  5  4  4 )
--    (  7  5  5  6  6  4  4  5  6  4  4  5  5  3  3  4 )
--    (  8  5  9  9  7  7  4  8  7  4  8  8  6  6  3  7 )
--    (  8  8  9  3  3  7  8  8  7  7  8  2  2  6  7  7 )
--    (  8  8  7  3  9 -1  8  8  7  7  6  6  2  8  7  7 )
--    (  8  6  7  7  9  9  7  8  7  5  5  6  8  8  6  7 )
--    (  9  6  6 10 10  7  7 11  8  8  5  9  9  6  6 10 )
--    (  9  9 10 10 10 10 11 11  1  8  9  9  9  9 10 10 )
--    (  8  8  7  7  7  7  6  1  1  9  8  8  8  8  7  7 )
--    (  8  6  6  7  7  5  6  6  9  9  7  8  8  6  6  7 )
--    (  9  6 10 10  8  5  5  9 10  7  7 11  9  9  6 10 )
--    (  9  9 10  4  8  8  9  9 10 10 11 11  5  9 10 10 )
--    (  9  9  8  4  4 10  9  9 10 10  9  5  5 11 10 10 )
--    (  9  7  8  8 10 10  8  9 10  8  9  9 11 11  9 10 )
--    ( 10  7  7 11 11  8  8 12 11  8  8 12 12  9  9 13 )
--    ( 10 10 11 11 11 11 12 12 11 11 12 12 12 12 13 13 )

triomino :: Int -> Posicion -> Tablero
triomino n p =
  divideVenceras ind resuelve divide combina (pbInicial n p)

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- + Raúl Ibáñez "Embaldosando con L-triominós (Un ejemplo de
--   demostración por inducción)" http://bit.ly/1DKPBbt
-- + "Algorithmic puzzles" pp. 10.

-- Programas interactivos
-- ======================
-- + "Interactive 8-by-8 Tromino Puzzle" http://bit.ly/1DKRNjn
-- + "Tromino Puzzle: Interactive Illustration of Golomb's Theorem"
--    http://bit.ly/1DKS0mL
