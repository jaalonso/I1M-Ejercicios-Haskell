-- El_triangulo_de_Floyd.hs
-- El triángulo de Floyd.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module El_triangulo_de_Floyd where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El triángulo de Floyd, llamado así en honor a Robert Floyd, es un
-- triángulo rectángulo formado con números naturales. Para crear un
-- triángulo de Floyd, se comienza con un 1 en la esquina superior
-- izquierda, y se continúa escribiendo la secuencia de los números
-- naturales de manera que cada línea contenga un número más que la
-- anterior. Las 5 primeras líneas del triángulo de Floyd son
--     1
--     2   3
--     4   5   6
--     7   8   9  10
--    11  12  13  14  15
--
-- El triángulo de Floyd tiene varias propiedades matemáticas
-- interesantes. Los números del cateto de la parte izquierda forman la
-- secuencia de los números poligonales centrales, mientras que los de
-- la hipotenusa nos dan el conjunto de los números triangulares.

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 0. Los números triangulares se forman como sigue
--    *     *      *
--         * *    * *
--               * * *
--    1     3      6
--
-- La sucesión de los números triangulares se obtiene sumando los
-- números naturales. Así, los 5 primeros números triangulares son
--     1 = 1
--     3 = 1+2
--     6 = 1+2+3
--    10 = 1+2+3+4
--    15 = 1+2+3+4+5
--
-- Definir la función
--    triangulares :: [Integer]
-- tal que triangulares es la lista de los números triangulares. Por
-- ejemplo,
--    take 10 triangulares     ==  [1,3,6,10,15,21,28,36,45,55]
--    triangulares !! 2000000  ==  2000003000001
-- ---------------------------------------------------------------------

-- 1ª definición
triangulares1 :: [Integer]
triangulares1 = 1 : [x+y | (x,y) <- zip [2..] triangulares]

-- 2ª definición
triangulares2 :: [Integer]
triangulares2 = scanl (+) 1 [2..]

-- 3ª definición (usando la fórmula de la suma de la progresión):
triangulares3 :: [Integer]
triangulares3 = [(n*(n+1)) `div` 2 | n <- [1..]]

-- Comparación de eficiencia
--    λ> triangulares1 !! 1000000
--    500001500001
--    (3.07 secs, 484,321,192 bytes)
--    λ> triangulares2 !! 1000000
--    500001500001
--    (0.04 secs, 0 bytes)
--    λ> triangulares3 !! 1000000
--    500001500001
--    (1.23 secs, 186,249,472 bytes)

-- En lo sucesivo, usaremos como triangulares la segunda definición.
triangulares :: [Integer]
triangulares = triangulares2

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    siguienteF :: [Integer] -> [Integer]
-- tal que (siguienteF xs) es la lista de los elementos de la línea xs en
-- el triángulo de Lloyd. Por ejemplo,
--    siguienteF [2,3]    ==  [4,5,6]
--    siguienteF [4,5,6]  ==  [7,8,9,10]
-- ---------------------------------------------------------------------

siguienteF :: [Integer] -> [Integer]
siguienteF xs = [a..a+n]
  where a = 1 + last xs
        n = genericLength xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    trianguloFloyd :: [[Integer]]
-- tal que trianguloFloyd es el triángulo de Floyd. Por ejemplo,
--    λ> take 4 trianguloFloyd
--    [[1],
--     [2,3],
--     [4,5,6],
--     [7,8,9,10]]
-- ---------------------------------------------------------------------

trianguloFloyd :: [[Integer]]
trianguloFloyd = iterate siguienteF [1]

-- Filas del triángulo de Floyd
-- ============================

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    filaTrianguloFloyd :: Integer -> [Integer]
-- tal que (filaTrianguloFloyd n) es la fila n-ésima del triángulo de
-- Floyd. Por ejemplo,
--    filaTrianguloFloyd 3  ==  [4,5,6]
--    filaTrianguloFloyd 4  ==  [7,8,9,10]
-- ---------------------------------------------------------------------

filaTrianguloFloyd :: Integer -> [Integer]
filaTrianguloFloyd n = trianguloFloyd `genericIndex` (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    sumaFilaTrianguloFloyd :: Integer -> Integer
-- tal que (sumaFilaTrianguloFloyd n) es la suma de los fila n-ésima del
-- triángulo de Floyd. Por ejemplo,
--    sumaFilaTrianguloFloyd 1  ==  1
--    sumaFilaTrianguloFloyd 2  ==  5
--    sumaFilaTrianguloFloyd 3  ==  15
--    sumaFilaTrianguloFloyd 4  ==  34
--    sumaFilaTrianguloFloyd 5  ==  65
-- ---------------------------------------------------------------------

sumaFilaTrianguloFloyd :: Integer -> Integer
sumaFilaTrianguloFloyd = sum . filaTrianguloFloyd

-- ---------------------------------------------------------------------
-- Ejercicio 5. A partir de los valores de (sumaFilaTrianguloFloyd n)
-- para n entre 1 y 5, conjeturar una fórmula para calcular
-- (sumaFilaTrianguloFloyd n).
-- ---------------------------------------------------------------------

-- Usando Wolfram Alpha (como se indica en http://wolfr.am/19XAl2X )
-- a partir de 1, 5, 15, 34, 65, ... se obtiene la fórmula
--    (n^3+n)/2

-- ---------------------------------------------------------------------
-- Ejecicio 6. Comprobar con QuickCheck la conjetura obtenida en el
-- ejercicio anterior.
-- ---------------------------------------------------------------------

-- La conjetura es
prop_sumaFilaTrianguloFloyd :: Integer -> Property
prop_sumaFilaTrianguloFloyd n =
  n > 0 ==> sum (filaTrianguloFloyd n) == (n^3+n) `div` 2

-- La comprobación es
--    λ> quickCheck prop_sumaFilaTrianguloFloyd
--    +++ OK, passed 100 tests.

-- Hipotenusa del triángulo de Floyd y números triangulares
-- ========================================================

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    hipotenusaFloyd :: [Integer]
-- tal que hipotenusaFloyd es la lista de los elementos de la hipotenusa
-- del triángulo de Floyd. Por ejemplo,
--    take 5 hipotenusaFloyd  ==  [1,3,6,10,15]
-- ---------------------------------------------------------------------

hipotenusaFloyd :: [Integer]
hipotenusaFloyd = map last trianguloFloyd

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    prop_hipotenusaFloyd :: Int -> Bool
-- tal que (prop_hipotenusaFloyd n) se verifica si los n primeros
-- elementos de la hipotenusa del triángulo de Floyd son los primeros n
-- números triangulares.
--
-- Comprobar la propiedad para los 1000 primeros elementos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_hipotenusaFloyd :: Int -> Bool
prop_hipotenusaFloyd n =
    take n hipotenusaFloyd == take n triangulares

-- La comprobación es
--    λ> prop_hipotenusaFloyd 1000
--    True
