-- Programacion_dinamica_Apilamiento_de_barriles.hs
-- Programación dinámica: Apilamiento de barriles.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Programacion_dinamica_Apilamiento_de_barriles where

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- Un montón de barriles se construye apilando unos encima de otros por
-- capas, de forma que en cada capa todos los barriles están apoyados
-- sobre dos de la capa inferior y todos los barriles de una misma capa
-- están pegados unos a otros. Por ejemplo, los siguientes montones son
-- válidos:
--       _          _   _                   _
--      / \        / \ / \                 / \
--     _\_/_      _\_/_\_/_   _       _   _\_/_   _
--    / \ / \    / \ / \ / \ / \     / \ / \ / \ / \
--    \_/ \_/    \_/ \_/ \_/ \_/     \_/ \_/ \_/ \_/
--
-- y los siguientes no son válidos:
--     _   _          _       _               _   _
--    / \ / \        / \     / \             / \ / \
--    \_/_\_/_      _\_/_   _\_/_       _   _\_/_\_/
--      / \ / \    / \ / \ / \ / \     / \ / \ / \
--      \_/ \_/    \_/ \_/ \_/ \_/     \_/ \_/ \_/
--
-- Se puede comprobar que el número M(n) de formas distintas de
-- construir montones con n barriles en la base viene dado por la
-- siguiente fórmula:
--               n-1
--              -------
--               \
--                \
--    M(n) = 1 +   )    (n-j) * M(j)
--                /
--               /
--              -------
--               j = 1
--
-- El objetivo de esta relación es estudiar la transformación de
-- definiciones recursivas en otras con programación dinámica y comparar
-- su eficiencia.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array

-- ---------------------------------------------------------------------
-- § Ejercicios                                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por recursión, la función
--    montonesR :: Integer -> Integer
-- tal que (montonesR n) es el número de formas distintas de construir
-- montones con n barriles en la base. Por ejemplo,
--    montonesR 1   ==  1
--    montonesR 5   ==  34
--    montonesR 10  ==  4181
--    montonesR 15  ==  514229
--    montonesR 20  ==  63245986
-- ---------------------------------------------------------------------

montonesR :: Integer -> Integer
montonesR 1 = 1
montonesR n = 1 + sum [(n-j) * montonesR j | j <- [1..n-1]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, por programación dinámica, la función
--    montonesPD :: Integer -> Integer
-- tal que (montonesPD n) es el número de formas distintas de construir
-- montones con n barriles en la base. Por ejemplo,
--    montonesPD 1   ==  1
--    montonesPD 5   ==  34
--    montonesPD 10  ==  4181
--    montonesPD 15  ==  514229
--    montonesPD 20  ==  63245986
--    length (show (montonesPD 1000))  ==  418
-- ---------------------------------------------------------------------

montonesPD :: Integer -> Integer
montonesPD n = vectorMontones n ! n

vectorMontones :: Integer -> Array Integer Integer
vectorMontones n = v where
  v = array (1,n) [(i,f i) | i <- [1..n]]
  f 1 = 1
  f k = 1 + sum [(k-j)*v!j | j <- [1..k-1]]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    montonesR  23
--    montonesPD 23
-- ---------------------------------------------------------------------

-- La comparación es
--    λ> montonesR 23
--    1134903170
--    (16.76 secs, 2,617,836,192 bytes)
--    λ> montonesPD 23
--    1134903170
--    (0.01 secs, 724,248 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Operando con las ecuaciones de M(n) se observa que
--    M(1) = 1                          = 1
--    M(2) = 1 + M(1)                   = M(1) + M(1)
--    M(3) = 1 + 2*M(1) + M(2)          = M(2) + (M(1) + M(2))
--    M(4) = 1 + 3*M(1) + 2*M(2) + M(3) = M(3) + (M(1) + M(2) + M(3))
-- En general,
--    M(n) = M(n-1) + (M(1) + ... + M(n-1))
--
-- Unsando la ecuación anterior, definir por recursión la función
--    montonesR2 :: Integer -> Integer
-- tal que (montonesR2 n) es el número de formas distintas de construir
-- montones con n barriles en la base. Por ejemplo,
--    montonesR2 1   ==  1
--    montonesR2 5   ==  34
--    montonesR2 10  ==  4181
--    montonesR2 15  ==  514229
--    montonesR2 20  ==  63245986
-- ---------------------------------------------------------------------

montonesR2 :: Integer -> Integer
montonesR2 = fst . montonesR2Aux

-- (montonesR2Aux n) es el par formado por M(n) y la suma
-- M(1)+...+M(n). Por ejemplo,
--    montonesR2Aux 10                  ==  (4181,6765)
--    montonesR 10                      ==  4181
--    sum [montonesR k | k <- [1..10]]  ==  6765
montonesR2Aux :: Integer -> (Integer,Integer)
montonesR2Aux 1 = (1,1)
montonesR2Aux n = (x+y,y+x+y)
  where (x,y) = montonesR2Aux (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    montonesR  23
--    montonesR2 23
--    length (show (montonesPD 1000))
--    length (show (montonesR2 1000))
-- ---------------------------------------------------------------------

-- La comparación es
--    λ> montonesR 23
--    1134903170
--    (16.76 secs, 2,617,836,192 bytes)
--    λ> montonesR2 23
--    1134903170
--    (0.01 secs, 602,104 bytes)
--    λ> length (show (montonesPD 1000))
--    418
--    (2.29 secs, 349,208,304 bytes)
--    λ> length (show (montonesR2 1000))
--    418
--    (0.01 secs, 1,600,192 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Usando la ecuación anterior y programación dinámica,
-- definir la función
--    montonesPD2 :: Integer -> Integer
-- tal que (montonesPD2 n) es el número de formas distintas de construir
-- montones con n barriles en la base. Por ejemplo,
--    montonesPD2 1   ==  1
--    montonesPD2 5   ==  34
--    montonesPD2 10  ==  4181
--    montonesPD2 15  ==  514229
--    montonesPD2 20  ==  63245986
-- ---------------------------------------------------------------------

montonesPD2 :: Integer -> Integer
montonesPD2 n = fst (vectorMontones2 n ! n)

vectorMontones2 :: Integer -> Array Integer (Integer,Integer)
vectorMontones2 n = v where
  v = array (1,n) [(i,f i) | i <- [1..n]]
  f 1 = (1,1)
  f k = (x+y,y+x+y)
    where (x,y) = v!(k-1)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    length (show (montonesR2  40000))
--    length (show (montonesPD2 40000))
-- ---------------------------------------------------------------------

-- La comparación es
--    λ> length (show (montonesR2 40000))
--    16719
--    (2.04 secs, 452,447,664 bytes)
--    λ> length (show (montonesPD2 40000))
--    16719
--    (2.12 secs, 466,528,472 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir, usando scanl1, la lista
--    sucMontones :: [Integer]
-- cuyos elementos son los números de formas distintas de construir
-- montones con n barriles en la base, para n = 1, 2, .... Por ejemplo,
--    take 10 sucMontones  ==  [1,2,5,13,34,89,233,610,1597,4181]
-- ---------------------------------------------------------------------

sucMontones :: [Integer]
sucMontones = 1 : zipWith (+) sucMontones (scanl1 (+) sucMontones)

-- El cálculo es
--    | sucMontones        | scanl1 (+) sucMontones |
--    | 1:...              | 1:...                  |
--    | 1:2:...            | 1:3:...                |
--    | 1:2:5:...          | 1:3:8:...              |
--    | 1:2:5:13:...       | 1:3:8:21:...           |
--    | 1:2:5:13:34:...    | 1:3:8:21:55:...        |
--    | 1:2:5:13:34:89:... | 1:3:8:21:55:144:...    |

-- ---------------------------------------------------------------------
-- Ejercicio 8. Usando la sucesión anterior, definir la función
--    montonesS :: Integer -> Integer
-- tal que (montonesS n) es el número de formas distintas de construir
-- montones con n barriles en la base. Por ejemplo,
--    montonesS 1   ==  1
--    montonesS 5   ==  34
--    montonesS 10  ==  4181
--    montonesS 15  ==  514229
--    montonesS 20  ==  63245986
-- ---------------------------------------------------------------------

montonesS :: Int -> Integer
montonesS n = sucMontones !! (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Comparar la eficiencia calculando el tiempo necesario
-- para evaluar las siguientes expresiones
--    length (show (montonesR2  40000))
--    length (show (montonesPD2 40000))
--    length (show (montonesS   40000))
-- ---------------------------------------------------------------------

-- La comparación es
--    λ> length (show (montonesR2 40000))
--    16719
--    (2.04 secs, 452,447,664 bytes)
--    λ> length (show (montonesPD2 40000))
--    16719
--    (2.12 secs, 466,528,472 bytes)
--    λ> length (show (montonesS 40000))
--    16719
--    (0.72 secs, 298,062,216 bytes)
