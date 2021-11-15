-- Resolucion_de_problemas_mediante_busqueda_en_espacios_de_estados.hs
-- Resolución de problemas mediante búsqueda en espacios de estados.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Resolucion_de_problemas_mediante_busqueda_en_espacios_de_estados where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es resolver problemas
-- mediante búsqueda en espacio de estados, utilizando las
-- implementaciones estudiadas en el tema 23 que se encuentra en
--    https://jaalonso.github.io/cursos/i1m/temas/tema-23.html
--
--
-- Para realizar los ejercicios hay que tener instalada la librería de
-- I1M. Para instalarla basta ejecutar en una consola
--    cabal update
--    cabal install I1M

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import I1M.BusquedaEnEspaciosDeEstados
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Las fichas del dominó se pueden representar por pares de
-- números enteros. El problema del dominó consiste en colocar todas las
-- fichas de una lista dada de forma que el segundo número de cada ficha
-- coincida con el primero de la siguiente.
--
-- Definir, mediante búsqueda en espacio de estados, la función
--    domino :: [(Int,Int)] -> [[(Int,Int)]]
-- tal que (domino fs) es la lista de las soluciones del problema del
-- dominó correspondiente a las fichas fs. Por ejemplo,
--    λ> domino [(1,2),(2,3),(1,4)]
--    [[(4,1),(1,2),(2,3)],[(3,2),(2,1),(1,4)]]
--    λ> domino [(1,2),(1,1),(1,4)]
--    [[(4,1),(1,1),(1,2)],[(2,1),(1,1),(1,4)]]
--    λ> domino [(1,2),(3,4),(2,3)]
--    [[(1,2),(2,3),(3,4)],[(4,3),(3,2),(2,1)]]
--    λ> domino [(1,2),(2,3),(5,4)]
--    []
-- ---------------------------------------------------------------------

-- Las fichas son pares de números enteros.
type Ficha  = (Int,Int)

-- Un problema está definido por la lista de fichas que hay que colocar
type Problema = [Ficha]

-- Los estados son los pares formados por la listas sin colocar y las
-- colocadas.
type Estado = ([Ficha],[Ficha])

-- (inicial p) es el estado inicial del problema p.
inicial :: Problema -> Estado
inicial p = (p,[])

-- (es final e) se verifica si e es un estado final.
esFinal :: Estado -> Bool
esFinal (fs,_) = null fs

sucesores :: Estado -> [Estado]
sucesores (fs,[]) =
  [(delete (a,b) fs, [(a,b)]) | (a,b) <- fs, a /= b] ++
  [(delete (a,b) fs, [(b,a)]) | (a,b) <- fs]
sucesores (fs,n@((x,_):_)) =
  [(delete (u,v) fs,(u,v):n) | (u,v) <- fs, u /= v, v == x] ++
  [(delete (u,v) fs,(v,u):n) | (u,v) <- fs, u /= v, u == x] ++
  [(delete (u,v) fs,(u,v):n) | (u,v) <- fs, u == v, u == x]

soluciones :: Problema -> [Estado]
soluciones ps = buscaEE sucesores
                        esFinal
                        (inicial ps)

domino :: Problema -> [[Ficha]]
domino ps = map snd (soluciones ps)

-- ---------------------------------------------------------------------
-- Ejercicio 2. El problema de suma cero consiste en, dado el conjunto
-- de números enteros, encontrar sus subconjuntos no vacío cuyos
-- elementos sumen cero.
--
-- Definir, mediante búsqueda en espacio de estados, la función
--    suma0 :: [Int] -> [[Int]]
-- tal que (suma0 ns) es la lista de las soluciones del problema de suma
-- cero para ns. Por ejemplo,
--    λ> suma0 [-7,-3,-2,5,8]
--    [[-3,-2,5]]
--    λ> suma0 [-7,-3,-2,5,8,-1]
--    [[-7,-3,-2,-1,5,8],[-7,-1,8],[-3,-2,5]]
--    λ> suma0 [-7,-3,1,5,8]
--    []
-- ---------------------------------------------------------------------


-- Los estados son ternas formadas por los números seleccionados, su
-- suma y los restantes números.
type EstadoSuma0 = ([Int], Int, [Int])

inicialSuma0 :: [Int] -> EstadoSuma0
inicialSuma0 ns = ([],0,ns)

esFinalSuma0 :: EstadoSuma0 -> Bool
esFinalSuma0 (xs,s,_) = not (null xs) && s == 0

sucesoresSuma0 :: EstadoSuma0 -> [EstadoSuma0]
sucesoresSuma0 (xs,s,ns) = [(n:xs, n+s, delete n ns) | n <- ns]

solucionesSuma0 :: [Int] -> [EstadoSuma0]
solucionesSuma0 ns = buscaEE sucesoresSuma0
                             esFinalSuma0
                             (inicialSuma0 ns)

suma0 :: [Int] -> [[Int]]
suma0 ns = nub [sort xs | (xs,_,_) <- solucionesSuma0 ns]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Se tienen dos jarras, una de 4 litros de capacidad y
-- otra de 3. Ninguna de ellas tiene marcas de medición. Se tiene una
-- bomba que permite llenar las jarras de agua. El problema de las
-- jarras consiste en determinar cómo se puede lograr tener exactamente
-- 2 litros de agua en la jarra de 4 litros de capacidad.
--
-- Definir, mediante búsqueda en espacio de estados, la función
--    jarras :: [[(Int,Int)]]
-- tal que su valor es la lista de las soluciones del problema de las
-- jarras, Por ejemplo,
--    λ> jarras !! 4
--    [(0,0),(4,0),(1,3),(1,0),(0,1),(4,1),(2,3)]
-- La interpretación de la solución es:
--    (0,0) se inicia con las dos jarras vacías,
--    (4,0) se llena la jarra de 4 con el grifo,
--    (1,3) se llena la de 3 con la de 4,
--    (1,0) se vacía la de 3,
--    (0,1) se pasa el contenido de la primera a la segunda,
--    (4,1) se llena la primera con el grifo,
--    (2,3) se llena la segunda con la primera.
--
-- Nota. No importa el orden en el que se generan las soluciones.
-- ---------------------------------------------------------------------

-- Un estado es una lista de dos números. El primero es el contenido de
-- la jarra de 4 litros y el segundo el de la de 3 litros.
type EstadoJarras = (Int,Int)

inicialJarras :: EstadoJarras
inicialJarras = (0,0)

esFinalJarras :: EstadoJarras -> Bool
esFinalJarras (x,_) = x == 2

sucesoresEjarras :: EstadoJarras -> [EstadoJarras]
sucesoresEjarras (x,y) =
  [(4,y) | x < 4] ++
  [(x,3) | y < 3] ++
  [(0,y) | x > 0] ++
  [(x,0) | y > 0] ++
  [(4,y-(4-x)) | x < 4, y > 0, x + y > 4] ++
  [(x-(3-y),3) | x > 0, y < 3, x + y > 3] ++
  [(x+y,0) | y > 0, x + y <= 4] ++
  [(0,x+y) | x > 0, x + y <= 3]

-- Los nodos son las soluciones parciales
type NodoJarras = [EstadoJarras]

inicialNjarras :: NodoJarras
inicialNjarras = [inicialJarras]

esFinalNjarras :: NodoJarras -> Bool
esFinalNjarras (e:_) = esFinalJarras e
esFinalNjarras _     = error "Imposible"

sucesoresNjarras :: NodoJarras -> [NodoJarras]
sucesoresNjarras n@(e:_) =
  [e':n | e' <- sucesoresEjarras e,
          e' `notElem` n]
sucesoresNjarras _ = error "Imposible"

solucionesJarras :: [NodoJarras]
solucionesJarras = buscaEE sucesoresNjarras
                           esFinalNjarras
                           inicialNjarras

jarras :: [[(Int,Int)]]
jarras = map reverse solucionesJarras
