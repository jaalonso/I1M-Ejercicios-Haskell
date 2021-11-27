-- La_sucesion_de_Kolakoski.hs
-- La sucesión de Kolakoski.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module La_sucesion_de_Kolakoski where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Dada una sucesión, su contadora es la sucesión de las longitudes de
-- de sus bloque de elementos consecutivos iguales. Por ejemplo, la
-- sucesión contadora de abbaaabbba es 12331; es decir; 1 vez la a,
-- 2 la b, 3 la a, 3 la b y 1 la a.
--
-- La sucesión de Kolakoski es una sucesión infinita de los símbolos 1 y
-- 2 que es su propia contadora. Los primeros términos de la sucesión
-- de Kolakoski son 1221121221221... que coincide con su contadora (es
-- decir, 1 vez el 1, 2 veces el 2, 2 veces el 1, ...).
--
-- En esta relación se define la sucesión de Kolakoski.

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Dados los símbolos a y b, la sucesión contadora de
--    abbaaabbba... =  a bb aaa bbb a ...
-- es
--    1233...       =  1 2  3   3...
-- es decir; 1 vez la a, 2 la b, 3 la a, 3 la b, 1 la a, ...
--
-- Definir la función
--    contadora :: Eq a => [a] -> [Int]
-- tal que (contadora xs) es la sucesión contadora de xs. Por ejemplo,
--    contadora "abbaaabbb"        ==  [1,2,3,3]
--    contadora "122112122121121"  ==  [1,2,2,1,1,2,1,1,2,1,1]
-- ---------------------------------------------------------------------

-- 1ª definición (usando group definida en Data.List)
contadora :: Eq a => [a] -> [Int]
contadora xs = map length (group xs)

-- 2ª definición (sin argumentos)
contadora2 :: Eq a => [a] -> [Int]
contadora2 = map length . group

-- 3ª definición (por recursión sin group):
contadora3 :: Eq a => [a] -> [Int]
contadora3 [] = []
contadora3 ys@(x:xs) =
  length (takeWhile (==x) ys) : contadora3 (dropWhile (==x) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    contada :: [Int] -> [a] -> [a]
-- tal que (contada ns xs) es la sucesión formada por los símbolos de xs
-- cuya contadora es ns. Por ejemplo,
--    contada [1,2,3,3] "ab"                ==  "abbaaabbb"
--    contada [1,2,3,3] "abc"               ==  "abbcccaaa"
--    contada [1,2,2,1,1,2,1,1,2,1,1] "12"  ==  "122112122121121"
-- ---------------------------------------------------------------------

contada :: [Int] -> [a] -> [a]
contada (n:ns) (x:xs) = replicate n x ++ contada ns (xs++[x])
contada _      _      = []

-- ---------------------------------------------------------------------
-- Ejercicio 3. La sucesión autocontadora (o sucesión de  Kolakoski) es
-- la sucesión xs formada por 1 y 2 tal que coincide con su contada; es
-- decir (contadora xs) == xs. Los primeros términos de la función
-- autocontadora son
--    1221121221221... = 1 22 11 2 1 22 1 22 11 ...
-- y su contadora es
--    122112122...     = 1 2  2  1 1 2  1 2  2...
-- que coincide con la inicial.
--
-- Definir la función
--    autocontadora :: [Int]
-- tal que autocontadora es la sucesión autocondadora con los números 1
-- y 2. Por ejemplo,
--    take 11 autocontadora  ==  [1,2,2,1,1,2,1,2,2,1,2]
--    take 12 autocontadora  ==  [1,2,2,1,1,2,1,2,2,1,2,2]
--    take 18 autocontadora  ==  [1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2]
-- ---------------------------------------------------------------------

-- 1ª solución
autocontadora :: [Int]
autocontadora = [1,2] ++ siguiente [2] 2

-- Los pasos lo da la función siguiente. Por ejemplo,
--    take 3 (siguiente [2] 2)            ==  [2,1,1]
--    take 4 (siguiente [2,1,1] 1)        ==  [2,1,1,2]
--    take 6 (siguiente [2,1,1,2] 2)      ==  [2,1,1,2,1,1]
--    take 7 (siguiente [2,1,1,2,1,1] 1)  ==  [2,1,1,2,1,1,2]
siguiente :: [Int] -> Int -> [Int]
siguiente (x:xs) y = x : siguiente (xs ++ nuevos x) y'
    where contrario 1 = 2
          contrario 2 = 1
          contrario _ = error "Imposible"
          y'          = contrario y
          nuevos 1    = [y']
          nuevos 2    = [y',y']
          nuevos _    = error "Imposible"
siguiente [] _ = error "Imposible"

-- 2ª solución (usando contada)
autocontadora2 :: [Int]
autocontadora2 = 1 : 2: xs
    where xs = 2 : contada xs [1,2]
