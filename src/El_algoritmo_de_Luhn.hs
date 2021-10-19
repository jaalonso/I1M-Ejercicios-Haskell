-- El_algoritmo_de_Luhn.hs
-- El algoritmo de Luhn
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module El_algoritmo_de_Luhn where

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es estudiar un algoritmo para validar
-- algunos identificadores numéricos como los números de algunas tarjetas
-- de crédito; por ejemplo, las de tipo Visa o Master Card.
--
-- El algoritmo que vamos a estudiar es el algoritmo de Luhn consistente
-- en aplicar los siguientes pasos a los dígitos del número de la
-- tarjeta.
--    1. Se invierten los dígitos del número; por ejemplo, [9,4,5,5] se
--       transforma en [5,5,4,9].
--    2. Se duplican los dígitos que se encuentra en posiciones impares
--       (empezando a contar en 0); por ejemplo, [5,5,4,9] se transforma
--       en [5,10,4,18].
--    3. Se suman los dígitos de cada número; por ejemplo, [5,10,4,18]
--       se transforma en 5 + (1 + 0) + 4 + (1 + 8) = 19.
--    4. Si el último dígito de la suma es 0, el número es válido; y no
--       lo es, en caso contrario.
--
-- A los números válidos, los llamaremos números de Luhn.

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    digitosInv :: Integer -> [Integer]
-- tal que (digitosInv n) es la lista de los dígitos del número n. en
-- orden inverso. Por ejemplo,
--    digitosInv 320274  ==  [4,7,2,0,2,3]
-- ---------------------------------------------------------------------

-- 1ª solución
digitosInv :: Integer -> [Integer]
digitosInv n
  | n < 10    = [n]
  | otherwise = (n `rem` 10) : digitosInv (n `div` 10)

-- 2ª solución
digitosInv2 :: Integer -> [Integer]
digitosInv2 n = [read [x] | x <- reverse (show n)]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    doblePosImpar :: [Integer] -> [Integer]
-- tal que (doblePosImpar ns) es la lista obtenida doblando los
-- elementos en las posiciones impares (empezando a contar en cero y
-- dejando igual a los que están en posiciones pares. Por ejemplo,
--    doblePosImpar [4,9,5,5]    ==  [4,18,5,10]
--    doblePosImpar [4,9,5,5,7]  ==  [4,18,5,10,7]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
doblePosImpar :: [Integer] -> [Integer]
doblePosImpar []       = []
doblePosImpar [x]      = [x]
doblePosImpar (x:y:zs) = x : 2*y : doblePosImpar zs

-- 2ª definición (por recursión)
doblePosImpar2 :: [Integer] -> [Integer]
doblePosImpar2 (x:y:zs) = x : 2*y : doblePosImpar2 zs
doblePosImpar2 xs       = xs

-- 3ª definición (por comprensión)
doblePosImpar3 :: [Integer] -> [Integer]
doblePosImpar3 xs = [f n x | (n,x) <- zip [0..] xs]
  where f n x | odd n     = 2*x
              | otherwise = x

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    sumaDigitos :: [Integer] -> Integer
-- tal que (sumaDigitos ns) es la suma de los dígitos de ns. Por
-- ejemplo,
--    sumaDigitos [10,5,18,4] = 1 + 0 + 5 + 1 + 8 + 4 =
--                            = 19
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
sumaDigitos :: [Integer] -> Integer
sumaDigitos ns = sum [sum (digitosInv n) | n <- ns]

-- 2ª definición (por recursión):
sumaDigitos2 :: [Integer] -> Integer
sumaDigitos2 []     = 0
sumaDigitos2 (n:ns) = sum (digitosInv n) + sumaDigitos2 ns

-- 3ª definición (con orden superior):
sumaDigitos3 :: [Integer] -> Integer
sumaDigitos3 = sum . map (sum . digitosInv)

-- 4ª definición (con plegado):
sumaDigitos4 :: [Integer] -> Integer
sumaDigitos4 = foldr ((+) . sum . digitosInv) 0

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    ultimoDigito :: Integer -> Integer
-- tal que (ultimoDigito n) es el último dígito de n. Por ejemplo,
--    ultimoDigito 123 == 3
--    ultimoDigito   0 == 0
-- ---------------------------------------------------------------------

ultimoDigito :: Integer -> Integer
ultimoDigito n = n `rem` 10

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    luhn :: Integer -> Bool
-- tal que (luhn n) se verifica si n es un número de Luhn. Por ejemplo,
--    luhn 5594589764218858  ==  True
--    luhn 1234567898765432  ==  False
-- ---------------------------------------------------------------------

-- 1ª solución
luhn :: Integer -> Bool
luhn n =
  ultimoDigito (sumaDigitos (doblePosImpar (digitosInv n))) == 0

-- 2ª solución
luhn2 :: Integer -> Bool
luhn2 =
  (==0) . ultimoDigito . sumaDigitos . doblePosImpar . digitosInv

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Esta relación es una adaptación del primer trabajo del curso "CIS 194:
-- Introduction to Haskell (Spring 2015)" de la Univ. de Pensilvania,
-- impartido por Noam Zilberstein. El trabajo se encuentra en
-- http://www.cis.upenn.edu/~cis194/hw/01-intro.pdf
--
-- En el artículo [Algoritmo de Luhn](http://bit.ly/1FGGWsC) de la
-- Wikipedia se encuentra información del algoritmo
