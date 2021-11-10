-- Programación_dinamica_Caminos_en_una_reticula.hs
-- Programación dinámica: Caminos en una retícula.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Programación_dinamica_Caminos_en_una_reticula where

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List (genericLength)
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Se considera una retícula con sus posiciones numeradas,
-- desde el vértice superior izquierdo, hacia la derecha y hacia
-- abajo. Por ejemplo, la retícula de dimensión 3x4 se numera como sigue:
--    |-------+-------+-------+-------|
--    | (1,1) | (1,2) | (1,3) | (1,4) |
--    | (2,1) | (2,2) | (2,3) | (2,4) |
--    | (3,1) | (3,2) | (3,3) | (3,4) |
--    |-------+-------+-------+-------|
--
-- Definir, por recursión, la función
--    caminosR :: (Int,Int) -> [[(Int,Int)]]
-- tal que (caminosR (m,n)) es la lista de los caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
--    λ> caminosR (2,3)
--    [[(1,1),(1,2),(1,3),(2,3)],
--     [(1,1),(1,2),(2,2),(2,3)],
--     [(1,1),(2,1),(2,2),(2,3)]]
--    λ> mapM_ print (caminosR (3,4))
--    [(1,1),(1,2),(1,3),(1,4),(2,4),(3,4)]
--    [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4)]
--    [(1,1),(1,2),(2,2),(2,3),(2,4),(3,4)]
--    [(1,1),(2,1),(2,2),(2,3),(2,4),(3,4)]
--    [(1,1),(1,2),(1,3),(2,3),(3,3),(3,4)]
--    [(1,1),(1,2),(2,2),(2,3),(3,3),(3,4)]
--    [(1,1),(2,1),(2,2),(2,3),(3,3),(3,4)]
--    [(1,1),(1,2),(2,2),(3,2),(3,3),(3,4)]
--    [(1,1),(2,1),(2,2),(3,2),(3,3),(3,4)]
--    [(1,1),(2,1),(3,1),(3,2),(3,3),(3,4)]
-- ---------------------------------------------------------------------

caminosR :: (Int,Int) -> [[(Int,Int)]]
caminosR p = map reverse (caminosRAux p)
  where
    caminosRAux (1,y) = [[(1,z) | z <- [y,y-1..1]]]
    caminosRAux (x,1) = [[(z,1) | z <- [x,x-1..1]]]
    caminosRAux (x,y) = [(x,y) : cs | cs <- caminosRAux (x-1,y) ++
                                            caminosRAux (x,y-1)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por programación dinámica, la función
--    caminosPD :: (Int,Int) -> [[(Int,Int)]]
-- tal que (caminosPD (m,n)) es la lista de los caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
--    λ> caminosPD (2,3)
--    [[(1,1),(1,2),(1,3),(2,3)],
--     [(1,1),(1,2),(2,2),(2,3)],
--     [(1,1),(2,1),(2,2),(2,3)]]
--    λ> mapM_ print (caminosPD (3,4))
--    [(1,1),(1,2),(1,3),(1,4),(2,4),(3,4)]
--    [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4)]
--    [(1,1),(1,2),(2,2),(2,3),(2,4),(3,4)]
--    [(1,1),(2,1),(2,2),(2,3),(2,4),(3,4)]
--    [(1,1),(1,2),(1,3),(2,3),(3,3),(3,4)]
--    [(1,1),(1,2),(2,2),(2,3),(3,3),(3,4)]
--    [(1,1),(2,1),(2,2),(2,3),(3,3),(3,4)]
--    [(1,1),(1,2),(2,2),(3,2),(3,3),(3,4)]
--    [(1,1),(2,1),(2,2),(3,2),(3,3),(3,4)]
--    [(1,1),(2,1),(3,1),(3,2),(3,3),(3,4)]
-- ---------------------------------------------------------------------

caminosPD :: (Int,Int) -> [[(Int,Int)]]
caminosPD p = map reverse (matrizCaminos p ! p)

matrizCaminos :: (Int,Int) -> Array (Int,Int) [[(Int,Int)]]
matrizCaminos (m,n) = q
  where
    q = array ((1,1),(m,n)) [((i,j),f i j) | i <- [1..m], j <- [1..n]]
    f 1 y = [[(1,z) | z <- [y,y-1..1]]]
    f x 1 = [[(z,1) | z <- [x,x-1..1]]]
    f x y = [(x,y) : cs | cs <- q!(x-1,y) ++ q!(x,y-1)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    length (head (caminosR (8,8)))
--    length (head (caminosR (8,8)))
--    maximum (head (caminosR (2000,2000)))
--    maximum (head (caminosPD (2000,2000)))
-- ---------------------------------------------------------------------

-- La comparación es
--    λ> length (head (caminosR (8,8)))
--    15
--    (0.02 secs, 504,056 bytes)
--    λ> length (head (caminosPD (8,8)))
--    15
--    (0.01 secs, 503,024 bytes)
--
--    λ> maximum (head (caminosR (2000,2000)))
--    (2000,2000)
--    (0.02 secs, 0 bytes)
--    λ> maximum (head (caminosPD (2000,2000)))
--    (2000,2000)
--    (1.30 secs, 199,077,664 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, usando caminosR, la función
--    nCaminosCR :: (Int,Int) -> Integer
-- tal que (nCaminosCR (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
--      nCaminosR (2,3)                        ==  3
--      nCaminosR (3,4)                        ==  10
-- ---------------------------------------------------------------------

nCaminosCR :: (Int,Int) -> Integer
nCaminosCR = genericLength . caminosR

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, usando caminosPD, la función
--    nCaminosCPD :: (Int,Int) -> Integer
-- tal que (nCaminosCPD (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
-- ---------------------------------------------------------------------

nCaminosCPD :: (Int,Int) -> Integer
nCaminosCPD = genericLength . caminosPD

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir, por recursión, la función
--    nCaminosR :: (Int,Int) -> Integer
-- tal que (nCaminosR (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
-- ---------------------------------------------------------------------

nCaminosR :: (Int,Int) -> Integer
nCaminosR (1,_) = 1
nCaminosR (_,1) = 1
nCaminosR (x,y) = nCaminosR (x-1,y) + nCaminosR (x,y-1)

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir, por programación dinámica, la función
--    nCaminosPD :: (Int,Int) -> Integer
-- tal que (nCaminosPD (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
--    nCaminosPD (3,4)  ==  10
-- ---------------------------------------------------------------------

nCaminosPD :: (Int,Int) -> Integer
nCaminosPD p = matrizNCaminos p ! p

matrizNCaminos :: (Int,Int) -> Array (Int,Int) Integer
matrizNCaminos (m,n) = q
  where
    q = array ((1,1),(m,n)) [((i,j),f i j) | i <- [1..m], j <- [1..n]]
    f 1 _ = 1
    f _ 1 = 1
    f x y = q!(x-1,y) + q!(x,y-1)

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Los caminos desde (1,1) a (m,n) son las permutaciones
-- con repetición de m-1 veces la A (abajo) y n-1 veces la D
-- (derecha). Por tanto, su  número es
--    ((m-1)+(n-1))! / (m-1)!*(n-1)!
--
-- Definir, con la fórmula anterior, la función
--    nCaminosF :: (Int,Int) -> Integer
-- tal que (nCaminosF (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
--    nCaminosF (8,8)  ==  3432
-- ---------------------------------------------------------------------

nCaminosF :: (Int,Int) -> Integer
nCaminosF (m,n) =
  fact ((m-1)+(n-1)) `div` (fact (m-1) * fact (n-1))

fact :: Int -> Integer
fact n = product [1..fromIntegral n]

-- ---------------------------------------------------------------------
-- Ejercicio 2.6. La fórmula anterior para el cálculo del número de
-- caminos se puede simplificar.
--
-- Definir, con la fórmula simplificada, la función
--    nCaminosFS :: (Int,Int) -> Integer
-- tal que (nCaminosFS (m,n)) es el número de caminos en la retícula de
-- dimensión mxn desde (1,1) hasta (m,n). Por ejemplo,
-- ---------------------------------------------------------------------

nCaminosFS :: (Int,Int) -> Integer
nCaminosFS (m,n) =
  product [a+1..a+b] `div` product [2..b]
  where m' = fromIntegral (m-1)
        n' = fromIntegral (n-1)
        a  = max m' n'
        b  = min m' n'

-- ---------------------------------------------------------------------
-- Ejercicio 2.7. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    nCaminosCR  (8,8)
--    nCaminosCPD (8,8)
--    nCaminosCR  (12,12)
--    nCaminosCPD (12,12)
--    nCaminosR   (12,12)
--    nCaminosPD  (12,12)
--    length (show (nCaminosPD (1000,1000)))
--    length (show (nCaminosF  (1000,1000)))
--    length (show (nCaminosFS (1000,1000)))
--    length (show (nCaminosF  (2*10^4,2*10^4)))
--    length (show (nCaminosFS (2*10^4,2*10^4)))
-- ---------------------------------------------------------------------

-- La comparación es
--    λ> nCaminosCR (8,8)
--    3432
--    (2.11 secs, 2,132,573,904 bytes)
--    λ> nCaminosCPD (8,8)
--    3432
--    (0.02 secs, 0 bytes)
--
--    λ> nCaminosCR (12,12)
--    705432
--    (18.24 secs, 3,778,889,608 bytes)
--    λ> nCaminosCPD (12,12)
--    705432
--    (3.56 secs, 548,213,968 bytes)
--    λ> nCaminosR (12,12)
--    705432
--    (2.12 secs, 278,911,248 bytes)
--    λ> nCaminosPD (12,12)
--    705432
--    (0.01 secs, 0 bytes)
--
--    λ> length (show (nCaminosPD (1000,1000)))
--    600
--    (4.88 secs, 693,774,912 bytes)
--    λ> length (show (nCaminosF (1000,1000)))
--    600
--    (0.01 secs, 0 bytes)
--    λ> length (show (nCaminosFS (1000,1000)))
--    600
--    (0.01 secs, 0 bytes)
--
--    λ> length (show (nCaminosF (2*10^4,2*10^4)))
--    12039
--    (8.01 secs, 2,376,767,288 bytes)
--    λ> length (show (nCaminosFS (2*10^4,2*10^4)))
--    12039
--    (2.84 secs, 836,245,992 bytes)
