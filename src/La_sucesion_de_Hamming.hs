-- La_sucesion_de_Hamming.hs
-- La sucesión de Hamming.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module La_sucesion_de_Hamming where

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.Numbers.Primes
import Test.QuickCheck
import Graphics.Gnuplot.Simple

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los números de Hamming forman una sucesión estrictamente
-- creciente de números que cumplen las siguientes condiciones:
-- + El número 1 está en la sucesión.
-- + Si x está en la sucesión, entonces 2x, 3x y 5x también están.
-- + Ningún otro número está en la sucesión.
--
-- Definir la sucesión
--    hamming :: [Integer]
-- cuyos elementos son los números de Hamming. Por ejemplo,
--    take 12 hamming == [1,2,3,4,5,6,8,9,10,12,15,16]
-- ---------------------------------------------------------------------

hamming :: [Integer]
hamming = 1 : mezcla3 [2*i | i <- hamming]
                      [3*i | i <- hamming]
                      [5*i | i <- hamming]

-- mezcla3 xs ys zs es la lista obtenida mezclando las listas ordenadas
-- xs, ys y zs y eliminando los elementos duplicados. Por ejemplo,
--    mezcla3 [2,4,6,8,10] [3,6,9,12] [5,10]  ==  [2,3,4,5,6,8,9,10,12]
mezcla3 :: Ord a => [a] -> [a] -> [a] -> [a]
mezcla3 xs ys zs = mezcla2 xs (mezcla2 ys zs)

-- mezcla2 xs ys zs es la lista obtenida mezclando las listas ordenadas
-- xs e ys y eliminando los elementos duplicados. Por ejemplo,
--    mezcla2 [2,4,6,8,10,12] [3,6,9,12]  ==  [2,3,4,6,8,9,10,12]
mezcla2 :: Ord a => [a] -> [a] -> [a]
mezcla2 p@(x:xs) q@(y:ys) | x < y     = x:mezcla2 xs q
                          | x > y     = y:mezcla2 p  ys
                          | otherwise = x:mezcla2 xs ys
mezcla2 []       ys                   = ys
mezcla2 xs       []                   = xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    divisoresPrimosEn :: Integer -> [Integer] -> Bool
-- tal que (divisoresPrimosEn x ys) se verifica si x puede expresarse
-- como un producto de potencias de elementos de la lista de números
-- primos ys. Por ejemplo,
--    divisoresPrimosEn 12 [2,3,5]  ==  True
--    divisoresPrimosEn 14 [2,3,5]  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
divisoresPrimosEn1 :: Integer -> [Integer] -> Bool
divisoresPrimosEn1 1 _  = True
divisoresPrimosEn1 _ [] = False
divisoresPrimosEn1 x (y:ys)
  | mod x y == 0 = divisoresPrimosEn1 (div x y) (y:ys)
  | otherwise    = divisoresPrimosEn1 x ys

-- 2ª definición (por comprensión)
divisoresPrimosEn2 :: Integer -> [Integer] -> Bool
divisoresPrimosEn2 x ys = and [elem y ys | y <- primeFactors x]

-- 3ª definición (por cuantificación)
divisoresPrimosEn :: Integer -> [Integer] -> Bool
divisoresPrimosEn x ys = all (`elem` ys) (primeFactors x)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, usando divisoresPrimosEn, la constante
--    hamming2 :: [Integer]
-- tal que hamming es la sucesión de Hamming. Por ejemplo,
--    take 12 hamming2  ==  [1,2,3,4,5,6,8,9,10,12,15,16]
-- ---------------------------------------------------------------------

hamming2 :: [Integer]
hamming2 = [x | x <- [1..], divisoresPrimosEn x [2,3,5]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Comparar los tiempos de cálculo de las siguientes
-- expresiones
--    hamming2 !! 400
--    hamming  !! 400
-- ---------------------------------------------------------------------

-- La comparación es
--    λ> hamming2 !! 400
--    312500
--    (30.06 secs, 15,804,885,776 bytes)
--    λ> hamming !! 400
--    312500
--    (0.01 secs, 800,984 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    cantidadHammingMenores :: Integer -> Int
-- tal que (cantidadHammingMenores x) es la cantidad de números de
-- Hamming menores que x. Por ejemplo,
--    cantidadHammingMenores 6  ==  5
--    cantidadHammingMenores 7  ==  6
--    cantidadHammingMenores 8  ==  6
-- ---------------------------------------------------------------------

cantidadHammingMenores :: Integer -> Int
cantidadHammingMenores x = length (takeWhile (<x) hamming)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    siguienteHamming :: Integer -> Integer
-- tal que (siguienteHamming x) es el menor número de la sucesión de
-- Hamming mayor que x. Por ejemplo,
--    siguienteHamming 6   ==  8
--    siguienteHamming 21  ==  24
-- ---------------------------------------------------------------------

siguienteHamming :: Integer -> Integer
siguienteHamming x = head (dropWhile (<=x) hamming)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    huecoHamming :: Integer -> [(Integer,Integer)]
-- tal que (huecoHamming n) es la lista de pares de números consecutivos
-- en la sucesión de Hamming cuya distancia es mayor que n. Por ejemplo,
--    take 4 (huecoHamming 2)   ==  [(12,15),(20,24),(27,30),(32,36)]
--    take 3 (huecoHamming 2)   ==  [(12,15),(20,24),(27,30)]
--    take 2 (huecoHamming 3)   ==  [(20,24),(32,36)]
--    head (huecoHamming 10)    ==  (108,120)
--    head (huecoHamming 1000)  ==  (34992,36000)
-- ---------------------------------------------------------------------

huecoHamming :: Integer -> [(Integer,Integer)]
huecoHamming n = [(x,y) | x <- hamming,
                          let y = siguienteHamming x,
                          y-x > n]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que para todo n, existen
-- pares de números consecutivos en la sucesión de Hamming cuya
-- distancia es mayor que n.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_Hamming :: Integer -> Bool
prop_Hamming n = huecoHamming n' /= []
  where n' = abs n

-- La comprobación es
--    λ> quickCheck prop_Hamming
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir el procedimiento
--    grafica_Hamming :: Int -> IO ()
-- tal que (grafica_Hamming n) dibuja la gráfica de los n primeros
-- términos de la sucesión de Hamming.
-- ---------------------------------------------------------------------

grafica_Hamming :: Int -> IO ()
grafica_Hamming n =
  plotList []
           (take n hamming)
