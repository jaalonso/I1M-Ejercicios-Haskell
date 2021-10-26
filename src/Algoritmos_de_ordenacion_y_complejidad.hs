-- Algoritmos_de_ordenacion_y_complejidad.hs
-- Algoritmos de ordenación y complejidad.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Algoritmos_de_ordenacion_y_complejidad where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es presentar una recopilación de los
-- algoritmos de ordenación y el estudio de su complejidadusando las
-- técnicas estudiadas en el tema
--    https://jaalonso.github.io/cursos/i1m/temas/tema-28.html

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- § Ordenación por selección                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación por selección se selecciona el menor elemento de xs y se
-- le añade a la ordenación por selección de los restantes. Por ejemplo,
-- para ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente:
--       ordenaPorSeleccion [3,1,4,1,5,9,2]
--     = 1 : ordenaPorSeleccion [3,4,1,5,9,2]
--     = 1 : 1 : ordenaPorSeleccion [3,4,5,9,2]
--     = 1 : 1 : 2 : ordenaPorSeleccion [3,4,5,9]
--     = 1 : 1 : 2 : 3 : ordenaPorSeleccion [4,5,9]
--     = 1 : 1 : 2 : 3 : 4 : ordenaPorSeleccion [5,9]
--     = 1 : 1 : 2 : 3 : 4 : 5 : ordenaPorSeleccion [9]
--     = 1 : 1 : 2 : 3 : 4 : 5 : 9 : ordenaPorSeleccion []
--     = 1 : 1 : 2 : 3 : 4 : 5 : 9 : []
--     = [1,1,2,3,4,5,9]
--
-- Definir la función
--    ordenaPorSeleccion :: Ord a => [a] -> [a]
-- tal que (ordenaPorSeleccion xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorSeleccion [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorSeleccion :: Ord a => [a] -> [a]
ordenaPorSeleccion [] = []
ordenaPorSeleccion xs = m : ordenaPorSeleccion (delete m xs)
  where m = minimum xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular los tiempos necesarios para calcular
--     let n = k in length (ordenaPorSeleccion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000].
--
-- ¿Cuál es el orden de complejidad de ordenaPorSeleccion?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.05
--    2000 | 0.25
--    3000 | 0.58
--    4000 | 1.13

-- La complejidad de ordenaPorSeleccion es O(n^2).
--
-- Las ecuaciones de recurrencia del coste de ordenaPorSeleccion son
--    T(0) = 1
--    T(n) = 1 + T(n-1) + 2n
-- Luego, T(n) = (n+1)^2 (ver http://bit.ly/1DGsMeW )

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    ordenaPorSeleccion2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorSeleccion2 xs) es la lista xs ordenada por el
-- algoritmo de selección, pero usando un acumulador. Por ejemplo,
--    ordenaPorSeleccion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorSeleccion2 :: Ord a => [a] -> [a]
ordenaPorSeleccion2 [] = []
ordenaPorSeleccion2 (x:xs) = aux xs x []
  where aux [] m r = m : ordenaPorSeleccion2 r
        aux (y:ys) m r | y < m     = aux ys y (m:r)
                       | otherwise = aux ys m (y:r)

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Calcular los tiempos necesarios para calcular
--    let n = k in length (ordenaPorSeleccion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.39
--    2000 | 1.53
--    3000 | 3.48
--    4000 | 6.35

-- ---------------------------------------------------------------------
-- § Ordenación rápida (Quicksort)                                    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación rápida se selecciona el primer elemento x de xs, se divide
-- los restantes en los menores o iguales que x y en los mayores que x,
-- se ordena cada una de las dos partes y se unen los resultados. Por
-- ejemplo, para ordenar la lista [3,1,4,1,5,9,2] el proceso es el
-- siguiente:
--       or [3,1,4,1,5,9,2]
--     = or [1,1,2] ++ [3] ++ or [4,5,9]
--     = (or [1] ++ [1] ++ or [2]) ++ [3] ++ (or [] ++ [4] ++ or [5,9])
--     = ((or [] ++ [1] ++ or []) ++ [1] ++ (or [] ++ [2] ++ or []))
--       ++ [3] ++ ([] ++ [4] ++ (or [] ++ [5] ++ or [9]))
--     = (([] ++ [1] ++ []) ++ [1] ++ ([] ++ [2] ++ []))
--       ++ [3] ++ ([4] ++ ([] ++ [5] ++ (or [] ++ [9] ++ or [])))
--     = ([1] ++ [1] ++ [2] ++
--       ++ [3] ++ ([4] ++ ([5] ++ (or [] ++ [9] ++ or [])))
--     = ([1] ++ [1] ++ [2] ++
--       ++ [3] ++ ([4] ++ ([5] ++ ([] ++ [9] ++ [])))
--     = ([1] ++ [1] ++ [2] ++
--       ++ [3] ++ ([4] ++ ([5] ++ [9]))
--     = [1,1,2,3,4,5,9]
--
-- Definir la función
--    ordenaRapida :: Ord a => [a] -> [a]
-- tal que (ordenaRapida xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaRapida [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaRapida :: Ord a => [a] -> [a]
ordenaRapida [] = []
ordenaRapida (x:xs) =
  ordenaRapida menores ++ [x] ++ ordenaRapida mayores
  where menores = [y | y <- xs, y <= x]
        mayores = [y | y <- xs, y >  x]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Calcular los tiempos necesarios para calcular
--     let n = k in length (ordenaRapida [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es el orden de complejidad de ordenaRapida?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 |  0.64
--    2000 |  2.57
--    3000 |  6.64
--    4000 | 12.33

-- La complejidad de ordenaRapida es O(n log(n)).

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir, usando un acumulador, la función
--    ordenaRapida2 :: Ord a => [a] -> [a]
-- tal que (ordenaRapida2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación rápida. Por ejemplo,
--    ordenaRapida2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaRapida2 :: Ord a => [a] -> [a]
ordenaRapida2 xs = aux xs []
  where aux [] s     = s
        aux (x:ys) s = aux menores (x : aux mayores s)
          where menores = [y | y <- ys, y <= x]
                mayores = [y | y <- ys, y >  x]

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Calcular los tiempos necesarios para calcular
--     let n = k in length (ordenaRapida2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 |  0.56
--    2000 |  2.42
--    3000 |  5.87
--    4000 | 10.93

-- ---------------------------------------------------------------------
-- § Ordenación por inserción                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación por inserción se selecciona el primer elemento x de xs, se
-- ordena el resto de xs y se inserta x en su lugar. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente:
--      ordenaPorInsercion [3,1,4,1,5,9,2]
--    = 3 : ordenaPorInsercion [1,4,1,5,9,2]
--    = 3 : 1 : ordenaPorInsercion [4,1,5,9,2]
--    = 3 : 1 : 4 : ordenaPorInsercion [1,5,9,2]
--    = 3 : 1 : 4 : 1 : ordenaPorInsercion [5,9,2]
--    = 3 : 1 : 4 : 1 : 5 : ordenaPorInsercion [9,2]
--    = 3 : 1 : 4 : 1 : 5 : 9 : ordenaPorInsercion [2]
--    = 3 : 1 : 4 : 1 : 5 : 9 : 2 : ordenaPorInsercion []
--    = 3 : 1 : 4 : 1 : 5 : 9 : 2 : []
--    = 3 : 1 : 4 : 1 : 5 : 9 : [2]
--    = 3 : 1 : 4 : 1 : 5 : [2,9]
--    = 3 : 1 : 4 : 1 : [2,5,9]
--    = 3 : 1 : 4 : [1,2,5,9]
--    = 3 : 1 : [1,2,4,5,9]
--    = 3 : [1,1,2,4,5,9]
--    = [1,1,2,3,4,5,9]
--
-- Definir la función
--    ordenaPorInsercion :: Ord a => [a] -> [a]
-- tal que (ordenaPorInsercion xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorInsercion [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorInsercion :: Ord a => [a] -> [a]
ordenaPorInsercion []     = []
ordenaPorInsercion (x:xs) = inserta x (ordenaPorInsercion xs)

-- (inserta x xs) inserta el elemento x después de los elementos de xs
-- que son menores o iguales que x. Por ejemplo,
--    inserta 5 [3,2,6,4]  ==  [3,2,5,6,4]
inserta :: Ord a => a -> [a] -> [a]
inserta y []                   = [y]
inserta y l@(x:xs) | y <= x    = y : l
                   | otherwise = x : inserta y xs

-- 2ª definición de inserta:
inserta2 :: Ord a => a -> [a] -> [a]
inserta2 x xs = takeWhile (<= x) xs ++ [x] ++ dropWhile (<=x) xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular
--     let n = k in length (ordenaPorInsercion [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorInsercion?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.39
--    2000 | 1.53
--    3000 | 3.49
--    4000 | 6.32

-- La complejidad de ordenaPorInsercion es O(n^2)
--
-- Las ecuaciones de recurrencia del coste de ordenaPorInsercion son
--    T(0) = 1
--    T(n) = n + T(n-1)
-- Luego, T(n) = 2n(n+1)+1 (ver https://bit.ly/2WWMP85 )

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir, por plegados, la función
--    ordenaPorInsercion2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorInsercion2 xs) es la lista obtenida ordenando xs
-- por el procedimiento de ordenación por inserción. Por ejemplo,
--    ordenaPorInsercion2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorInsercion2 :: Ord a => [a] -> [a]
ordenaPorInsercion2 = foldr inserta []

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular los tiempos necesarios para calcular
--     let n = k in length (ordenaPorInsercion2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+------
--    1000 | 0.38
--    2000 | 1.54
--    3000 | 3.46
--    4000 | 6.29

-- ---------------------------------------------------------------------
-- § Ordenación por mezcla ("Mergesort")                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Para ordenar una lista xs mediante el algoritmo de
-- ordenación por mezcla se divide xs por la mitad, se ordena cada una
-- de las partes y se mezclan los resultados. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente:
--      om [3,1,4,1,5,9,2]
--    = m (om [3,1,4]) (om 1,5,9,2])
--    = m (m (om [3]) (om [1,4])) (m (om [1,5]) (om [9,2]))
--    = m (m [3] (m (om [1]) (om [4])))
--        (m (m (om [1]) (om [5])) (m (om [9]) (om [2])))
--    = m (m [3] (m [1] [4]))
--        (m (m [1] [5]) (m [9] [2]))
--    = m (m [3] [1,4]) (m [1,5] [2,9])
--    = m [1,3,4] [1,2,5,9]
--    = [1,1,2,3,4,5,9]
-- donde om es ordenaPorMezcla y m es mezcla.
--
-- Definir la función
--    ordenaPorMezcla :: Ord a => [a] -> [a]
-- tal que (ordenaPorMezcla xs) es la lista obtenida ordenando por
-- selección la lista xs. Por ejemplo,
--    ordenaPorMezcla [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorMezcla :: Ord a => [a] -> [a]
ordenaPorMezcla []  = []
ordenaPorMezcla [x] = [x]
ordenaPorMezcla l   = mezcla (ordenaPorMezcla l1) (ordenaPorMezcla l2)
    where l1 = take k l
          l2 = drop k l
          k  = length l `div` 2

-- (mezcla xs ys) es la lista obtenida mezclando xs e ys. Por ejemplo,
--    mezcla [1,3] [2,4,6]  ==  [1,2,3,4,6]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] b = b
mezcla a [] = a
mezcla a@(x:xs) b@(y:ys) | x <= y    = x : mezcla xs b
                         | otherwise = y : mezcla a ys

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Calcular los tiempos necesarios para calcular
--     let n = k in length (ordenaPorMezcla [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
--
-- ¿Cuál es la complejidad de ordenaPorMezcla?
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.02
--    2000 | 0.03
--    3000 | 0.05
--    4000 | 0.06

-- La complejidad de ordenaPorMezcla es O(n log(n)).
--
-- Las ecuaciones de recurrencia del coste de ordenaPorMezcla son
--    T(0) = 1
--    T(1) = 1
--    T(n) = n + 2*T(n/2)
-- Luego, T(n) = (c*n)/2+(n log(n))/(log(2)) (ver http://bit.ly/1EyUTYG )

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Otra forma de ordenar una lista xs mediante el
-- algoritmo de ordenación por mezcla consiste en dividir xs en listas
-- unitarias y mezclar los resultados. Por ejemplo, para
-- ordenar la lista [3,1,4,1,5,9,2] el proceso es el siguiente:
--      om [3,1,4,1,5,9,2]
--    = mp [[3],[1],[4],[1],[5],[9],[2]]
--    = mp [[1,3],[1,4],[5,9],[2]]
--    = mp [[1,1,3,4],[2,5,9]]
--    = [1,1,2,3,4,5,9]
-- donde om es ordenaPorMezcla y mp es mezclaPares.
--
-- Definir la función
--    ordenaPorMezcla2 :: Ord a => [a] -> [a]
-- tal que (ordenaPorMezcla2 xs) es la lista obtenida ordenando por
-- mezcla la lista xs. Por ejemplo,
--    ordenaPorMezcla2 [3,1,4,1,5,9,2]  ==  [1,1,2,3,4,5,9]
-- ---------------------------------------------------------------------

ordenaPorMezcla2 :: Ord a => [a] -> [a]
ordenaPorMezcla2 xs = aux (divide xs)
  where aux [r] = r
        aux ys  = aux (mezclaPares ys)

-- (divide xs) es la lista de de las listas unitarias formadas por los
-- elementos de xs. Por ejemplo,
--    divide [3,1,4,1,5,9,2,8]  ==  [[3],[1],[4],[1],[5],[9],[2],[8]]
divide :: Ord a => [a] -> [[a]]
divide xs = [[x] | x <- xs]

-- También se puede definir por recursión
divide2 :: Ord a => [a] -> [[a]]
divide2 []     = []
divide2 (x:xs) = [x] : divide2 xs

-- (mezclaPares xs) es la lista obtenida mezclando los pares de
-- elementos consecutivos de xs. Por ejemplo,
--    ghci> mezclaPares [[3],[1],[4],[1],[5],[9],[2],[8]]
--    [[1,3],[1,4],[5,9],[2,8]]
--    ghci> mezclaPares [[1,3],[1,4],[5,9],[2,8]]
--    [[1,1,3,4],[2,5,8,9]]
--    ghci> mezclaPares [[1,1,3,4],[2,5,8,9]]
--    [[1,1,2,3,4,5,8,9]]
--    ghci> mezclaPares [[1],[3],[2]]
--    [[1,3],[2]]
mezclaPares :: (Ord a) => [[a]] -> [[a]]
mezclaPares []           = []
mezclaPares [x]          = [x]
mezclaPares (xs:ys:zss)  = mezcla xs ys : mezclaPares zss

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Calcular los tiempos necesarios para calcular
--     let n = k in length (ordenaPorMezcla2 [n,n-1..1])
-- para k en [1000, 2000, 3000, 4000]
-- ---------------------------------------------------------------------

-- El resumen de los tiempos es
--    k    | segs.
--    -----+-----
--    1000 | 0.02
--    2000 | 0.03
--    3000 | 0.03
--    4000 | 0.05

-- ---------------------------------------------------------------------
-- § Comparaciones con listas aleatorias                              --
-- ---------------------------------------------------------------------

--    λ> import System.Random (randomRIO)
--    λ> import Control.Monad (replicateM)
--    λ> ej10000 <- replicateM 10000 (randomRIO (0,10000))
--    λ> :set +s
--    λ> maximum (ordenaPorSeleccion ej10000)
--    9998
--    (2.69 secs, 6,757,883,928 bytes)
--    λ> maximum (ordenaRapida ej10000)
--    9998
--    (0.11 secs, 40,701,576 bytes)
--    λ> maximum (ordenaPorInsercion ej10000)
--    9998
--    (6.57 secs, 6,305,208,920 bytes)
--    λ> maximum (ordenaPorMezcla ej10000)
--    9998
--    (0.09 secs, 36,797,672 bytes)
--    λ> ej20000 <- replicateM 20000 (randomRIO (0,20000))
--    (0.01 secs, 11,782,488 bytes)
--    λ> maximum (ordenaRapida ej20000)
--    20000
--    (0.18 secs, 86,766,376 bytes)
--    λ> maximum (ordenaPorMezcla ej20000)
--    20000
--    (0.14 secs, 78,188,176 bytes)
--    λ> ej50000 <- replicateM 50000 (randomRIO (0,50000))
--    (0.03 secs, 28,855,672 bytes)
--    λ> maximum (ordenaRapida ej50000)
--    50000
--    (0.45 secs, 240,099,608 bytes)
--    λ> maximum (ordenaPorMezcla ej50000)
--    50000
--    (0.38 secs, 211,648,672 bytes)
