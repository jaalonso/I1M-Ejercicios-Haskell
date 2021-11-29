-- Camino_de_maxima_suma_en_una_matriz.hs
-- Camino de máxima suma en una matriz.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Camino_de_maxima_suma_en_una_matriz where

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los caminos desde el extremo superior izquierdo
-- (posición (1,1)) hasta el extremo inferior derecho (posición (3,4))
-- en la matriz
--    (  1  6 11  2 )
--    (  7 12  3  8 )
--    (  3  8  4  9 )
-- moviéndose en cada paso una casilla hacia abajo o hacia la derecha,
-- son los siguientes:
--    1, 7,  3, 8, 4, 9
--    1, 7, 12, 8, 4, 9
--    1, 7, 12, 3, 4, 9
--    1, 7, 12, 3, 8, 9
--    1, 6, 12, 8, 4, 9
--    1, 6, 12, 3, 4, 9
--    1, 6, 12, 3, 8, 9
--    1, 6, 11, 3, 4, 9
--    1, 6, 11, 3, 8, 9
--    1, 6, 11, 2, 8, 9
-- La suma de los caminos son 32, 41, 36, 40, 40, 35, 39, 34, 38 y 37,
-- respectivamente. El camino de máxima suma es el segundo (1, 7, 12, 8,
-- 4, 9) que tiene una suma de 41.
--
-- Definir la función
--    caminos :: Matrix Int -> [[Int]]
-- tal que (caminos m) es la lista de los caminos en la matriz m desde
-- el extremo superior izquierdo hasta el extremo inferior derecho,
-- moviéndose en cada paso una casilla hacia abajo o hacia la
-- derecha. Por ejemplo,
--    λ> caminos (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
--    [[1,7, 3,8,4,9],
--     [1,7,12,8,4,9],
--     [1,7,12,3,4,9],
--     [1,7,12,3,8,9],
--     [1,6,12,8,4,9],
--     [1,6,12,3,4,9],
--     [1,6,12,3,8,9],
--     [1,6,11,3,4,9],
--     [1,6,11,3,8,9],
--     [1,6,11,2,8,9]]
--    λ> length (caminos (fromList 12 12 [1..]))
--    705432
-- ---------------------------------------------------------------------


-- 1ª definición de caminos (por espacios de estados)
-- --------------------------------------------------

caminos1 :: Matrix Int -> [[Int]]
caminos1 m =
  [[m!p | p <- reverse ps]
   | ps <- caminosReticula (nrows m, ncols m)]

-- (caminos (m,n)) es la lista de los caminos en la retícula de dimensión
-- mxn desde (1,1) hasta (m,n) en los que los movimientos que se
-- permiten son una casilla hacia abajo o hacia la derecha. Por ejemplo,
--    λ> caminos (2,3)
--    [[(1,1),(1,2),(1,3),(2,3)],
--     [(1,1),(1,2),(2,2),(2,3)],
--     [(1,1),(2,1),(2,2),(2,3)]]
caminosReticula :: (Int,Int) -> [[(Int,Int)]]
caminosReticula p = busca [inicial]
  where
    busca []        = []
    busca (e:es)
      | esFinal p e = e : busca es
      | otherwise   = busca (es ++ sucesores p e)

-- Un estado es una lista de posiciones (en orden inverso, desde la
-- (1,1) hasta la actual).
type Estado = [(Int,Int)]

-- inicial es el estado inicial del problema.
inicial :: Estado
inicial = [(1,1)]

-- (esFinalp e) es verifica si e es un estado final del problema
-- p. Por ejemplo,
--    esFinal (2,3) [(2,3),(2,2),(2,1),(1,1)]  ==  True
--    esFinal (2,3) [(2,2),(2,1),(1,1)]        ==  False
esFinal :: (Int,Int) -> Estado -> Bool
esFinal p (q:_) = p == q
esFinal _ _     = error "Imposible"

-- (sucesores p e) es la lista de los sucesores del estado e en el
-- problema p. Por ejemplo,
--    sucesores (2,3) [(1,1)]              ==  [[(2,1),(1,1)],[(1,2),(1,1)]]
--    sucesores (2,3) [(2,2),(2,1),(1,1)]  ==  [[(2,3),(2,2),(2,1),(1,1)]]
sucesores :: (Int,Int) -> Estado -> [Estado]
sucesores (m,n) e@((x,y):_) =
     [(x+1,y):e | x < m]
  ++ [(x,y+1):e | y < n]
sucesores _ _ = error "Imposible"

-- 2ª definición de caminos (por recursión)
-- ----------------------------------------

caminos2 :: Matrix Int -> [[Int]]
caminos2 m =
  map reverse (caminos2Aux m (nf,nc))
  where nf = nrows m
        nc = ncols m

-- (caminos2Aux p x) es la lista de los caminos invertidos en la matriz p
-- desde la posición (1,1) hasta la posicioń x. Por ejemplo,
caminos2Aux :: Matrix Int -> (Int,Int) -> [[Int]]
caminos2Aux m (1,1) = [[m!(1,1)]]
caminos2Aux m (1,j) = [[m!(1,k) | k <- [j,j-1..1]]]
caminos2Aux m (i,1) = [[m!(k,1) | k <- [i,i-1..1]]]
caminos2Aux m (i,j) = [m!(i,j) : xs
                      | xs <- caminos2Aux m (i,j-1) ++
                              caminos2Aux m (i-1,j)]

-- 3ª solución (mediante programación dinámica)
-- --------------------------------------------

caminos3 :: Matrix Int -> [[Int]]
caminos3 m =
  map reverse (matrizCaminos m ! (nrows m, ncols m))

matrizCaminos :: Matrix Int -> Matrix [[Int]]
matrizCaminos m = q
  where
    q = matrix (nrows m) (ncols m) f
    f (1,y) = [[m!(1,z) | z <- [y,y-1..1]]]
    f (x,1) = [[m!(z,1) | z <- [x,x-1..1]]]
    f (x,y) = [m!(x,y) : cs | cs <- q!(x-1,y) ++ q!(x,y-1)]

-- Nota: (caminos3 m) es la inversa de (caminos2 m).

-- Comparación de eficiencia
-- -------------------------

--    λ> length (caminos1 (fromList 8 8 [1..]))
--    3432
--    (2.12 secs, 2,077,988,976 bytes)
--    λ> length (caminos2 (fromList 8 8 [1..]))
--    3432
--    (0.04 secs, 0 bytes)
--
--    λ> length (caminos2 (fromList 11 11 [1..]))
--    184756
--    (3.64 secs, 667,727,568 bytes)
--    λ> length (caminos3 (fromList 11 11 [1..]))
--    184756
--    (0.82 secs, 129,181,072 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    maximaSuma :: Matrix Int -> Int
-- tal que (maximaSuma m) es el máximo de las sumas de los caminos en la
-- matriz m desde el extremo superior izquierdo hasta el extremo
-- inferior derecho, moviéndose en cada paso una casilla hacia abajo o
-- hacia la derecha. Por ejemplo,
--    λ> maximaSuma (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
--    41
--    λ> maximaSuma (fromList 800 800 [1..])
--    766721999
-- ---------------------------------------------------------------------

-- 1ª definicion de maximaSuma (con caminos1)
-- ------------------------------------------

maximaSuma1 :: Matrix Int -> Int
maximaSuma1 m =
  maximum (map sum (caminos1 m))

-- La definición anterior se puede puede simplificar:
maximaSuma1a :: Matrix Int -> Int
maximaSuma1a =
  maximum . map sum . caminos1

-- 2ª definición de maximaSuma (con caminos2)
-- ------------------------------------------

maximaSuma2 :: Matrix Int -> Int
maximaSuma2 m =
  maximum (map sum (caminos2 m))

-- La definición anterior se puede puede simplificar:
maximaSuma2a :: Matrix Int -> Int
maximaSuma2a =
  maximum . map sum . caminos2

-- 3ª definición de maximaSuma (con caminos2)
-- ------------------------------------------

maximaSuma3 :: Matrix Int -> Int
maximaSuma3 m =
  maximum (map sum (caminos3 m))

-- La definición anterior se puede puede simplificar:
maximaSuma3a :: Matrix Int -> Int
maximaSuma3a =
  maximum . map sum . caminos3

-- 4ª definicion de maximaSuma (por recursión)
-- -------------------------------------------

maximaSuma4 :: Matrix Int -> Int
maximaSuma4 m = maximaSuma4Aux m (nf,nc)
  where nf = nrows m
        nc = ncols m

-- (maximaSuma4Aux m p) calcula la suma máxima de un camino hasta la
-- posición p. Por ejemplo,
--    λ> maximaSuma4Aux (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) (3,4)
--    41
--    λ> maximaSuma4Aux (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) (3,3)
--    32
--    λ> maximaSuma4Aux (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) (2,4)
--    31
maximaSuma4Aux :: Matrix Int -> (Int,Int) -> Int
maximaSuma4Aux m (1,1) = m ! (1,1)
maximaSuma4Aux m (1,j) = maximaSuma4Aux m (1,j-1) + m ! (1,j)
maximaSuma4Aux m (i,1) = maximaSuma4Aux m (i-1,1) + m ! (i,1)
maximaSuma4Aux m (i,j) =
  max (maximaSuma4Aux m (i,j-1)) (maximaSuma4Aux m (i-1,j)) + m ! (i,j)

-- 5ª solución (mediante programación dinámica)
-- --------------------------------------------

maximaSuma5 :: Matrix Int -> Int
maximaSuma5 m = q ! (nf,nc)
  where nf = nrows m
        nc = ncols m
        q  = matrizMaximaSuma m

-- (matrizMaximaSuma m) es la matriz donde en cada posición p se
-- encuentra el máxima de las sumas de los caminos desde (1,1) a p en la
-- matriz m. Por ejemplo,
--    λ> matrizMaximaSuma (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
--    (  1  7 18 20 )
--    (  8 20 23 31 )
--    ( 11 28 32 41 )
matrizMaximaSuma :: Matrix Int -> Matrix Int
matrizMaximaSuma m = q
  where nf = nrows m
        nc = ncols m
        q  = matrix nf nc f
          where  f (1,1) = m ! (1,1)
                 f (1,j) = q ! (1,j-1) + m ! (1,j)
                 f (i,1) = q ! (i-1,1) + m ! (i,1)
                 f (i,j) = max (q ! (i,j-1)) (q ! (i-1,j)) + m ! (i,j)

-- Comparación de eficiencia
-- -------------------------

--    λ> maximaSuma1 (fromList 8 8 [1..])
--    659
--    (2.26 secs, 2,077,262,504 bytes)
--    λ> maximaSuma1a (fromList 8 8 [1..])
--    659
--    (2.23 secs, 2,077,350,928 bytes)
--    λ> maximaSuma2 (fromList 8 8 [1..])
--    659
--    (0.11 secs, 31,853,136 bytes)
--    λ> maximaSuma2a (fromList 8 8 [1..])
--    659
--    (0.09 secs, 19,952,640 bytes)
--
--    λ> maximaSuma2 (fromList 10 10 [1..])
--    1324
--    (2.25 secs, 349,722,744 bytes)
--    λ> maximaSuma3 (fromList 10 10 [1..])
--    1324
--    (0.76 secs, 151,019,296 bytes)
--
--    λ> maximaSuma3 (fromList 11 11 [1..])
--    1781
--    (3.02 secs, 545,659,632 bytes)
--    λ> maximaSuma4 (fromList 11 11 [1..])
--    1781
--    (1.57 secs, 210,124,912 bytes)
--
--    λ> maximaSuma4 (fromList 12 12 [1..])
--    2333
--    (5.60 secs, 810,739,032 bytes)
--    λ> maximaSuma5 (fromList 12 12 [1..])
--    2333
--    (0.01 secs, 23,154,776 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    caminoMaxSuma :: Matrix Int -> [Int]
-- tal que (caminoMaxSuma m) es un camino de máxima suma en la matriz m
-- desde el extremo superior izquierdo hasta el extremo inferior derecho,
-- moviéndose en cada paso una casilla hacia abajo o hacia la
-- derecha. Por ejemplo,
--    λ> caminoMaxSuma (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
--    [1,7,12,8,4,9]
--    λ> sum (caminoMaxSuma (fromList 500 500 [1..]))
--    187001249
-- ---------------------------------------------------------------------

-- 1ª definición de caminoMaxSuma (con caminos1)
-- ---------------------------------------------

caminoMaxSuma1 :: Matrix Int -> [Int]
caminoMaxSuma1 m =
  head [c | c <- cs, sum c == k]
  where cs = caminos1 m
        k  = maximum (map sum cs)

-- 2ª definición de caminoMaxSuma (con caminos1)
-- ---------------------------------------------

caminoMaxSuma2 :: Matrix Int -> [Int]
caminoMaxSuma2 m =
  head [c | c <- cs, sum c == k]
  where cs = caminos2 m
        k  = maximum (map sum cs)

-- 3ª definición de caminoMaxSuma (con caminos1)
-- ---------------------------------------------

caminoMaxSuma3 :: Matrix Int -> [Int]
caminoMaxSuma3 m =
  head [c | c <- cs, sum c == k]
  where cs = caminos3 m
        k  = maximum (map sum cs)

-- 4ª definición de caminoMaxSuma (con programación dinámica)
-- ---------------------------------------------------------

caminoMaxSuma4 :: Matrix Int -> [Int]
caminoMaxSuma4 m = reverse (snd (q ! (nf,nc)))
  where nf = nrows m
        nc = ncols m
        q  = caminoMaxSumaAux m

caminoMaxSumaAux :: Matrix Int -> Matrix (Int,[Int])
caminoMaxSumaAux m = q
  where
    nf = nrows m
    nc = ncols m
    q  = matrix nf nc f
      where
        f (1,1) = (m!(1,1),[m!(1,1)])
        f (1,j) = (k + m!(1,j), m!(1,j):xs)
          where (k,xs) = q!(1,j-1)
        f (i,1) = (k + m!(i,1), m!(i,1):xs)
          where (k,xs) = q!(i-1,1)
        f (i,j) | k1 > k2   = (k1 + m!(i,j), m!(i,j):xs)
                | otherwise = (k2 + m!(i,j), m!(i,j):ys)
          where (k1,xs) = q!(i,j-1)
                (k2,ys) = q!(i-1,j)

-- Comparación de eficiencia
-- -------------------------

--    λ> length (caminoMaxSuma1 (fromList 8 8 [1..]))
--    15
--    (2.22 secs, 2,082,168,848 bytes)
--    λ> length (caminoMaxSuma2 (fromList 8 8 [1..]))
--    15
--    (0.09 secs, 0 bytes)
--
--    λ> length (caminoMaxSuma2 (fromList 11 11 [1..]))
--    21
--    (10.00 secs, 1,510,120,328 bytes)
--    λ> length (caminoMaxSuma3 (fromList 11 11 [1..]))
--    21
--    (3.84 secs, 745,918,544 bytes)
--    λ> length (caminoMaxSuma4 (fromList 11 11 [1..]))
--    21
--    (0.01 secs, 0 bytes)
