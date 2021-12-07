-- Definiciones_por_comprension_con_cadenas_El_cifrado_Cesar.hs
-- Definiciones por comprensión con cadenas: El cifrado César.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Definiciones_por_comprension_con_cadenas_El_cifrado_Cesar where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En el tema 5, cuyas transparencias se encuentran en
--    https://jaalonso.github.io/cursos/i1m/temas/tema-5.html
-- se estudió, como aplicación de las definiciones por comprennsión, el
-- cifrado César. El objetivo de esta relación es modificar el programa
-- de cifrado César para que pueda utilizar también letras
-- mayúsculas. Por ejemplo,
--    λ> descifra "Ytit Ufwf Sfif"
--    "Todo Para Nada"
-- Para ello, se propone la modificación de las funciones correspondientes
-- del tema 5.

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Data.Char
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    minuscula2int :: Char -> Int
-- tal que (minuscula2int c) es el entero correspondiente a la letra
-- minúscula c. Por ejemplo,
--    minuscula2int 'a'  ==  0
--    minuscula2int 'd'  ==  3
--    minuscula2int 'z'  ==  25
-- ---------------------------------------------------------------------

minuscula2int :: Char -> Int
minuscula2int c = ord c - ord 'a'

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    mayuscula2int :: Char -> Int
-- tal que (mayuscula2int c) es el entero correspondiente a la letra
-- mayúscula c. Por ejemplo,
--    mayuscula2int 'A'  ==  0
--    mayuscula2int 'D'  ==  3
--    mayuscula2int 'Z'  ==  25
-- ---------------------------------------------------------------------

mayuscula2int :: Char -> Int
mayuscula2int c = ord c - ord 'A'

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    int2minuscula :: Int -> Char
-- tal que (int2minuscula n) es la letra minúscula correspondiente al
-- entero n. Por ejemplo,
--    int2minuscula 0   ==  'a'
--    int2minuscula 3   ==  'd'
--    int2minuscula 25  ==  'z'
-- ---------------------------------------------------------------------

int2minuscula :: Int -> Char
int2minuscula n = chr (ord 'a' + n)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    int2mayuscula :: Int -> Char
-- tal que (int2mayuscula n) es la letra mayúscula correspondiente al
-- entero n. Por ejemplo,
--    int2mayuscula 0   ==  'A'
--    int2mayuscula 3   ==  'D'
--    int2mayuscula 25  ==  'Z'
-- ---------------------------------------------------------------------

int2mayuscula :: Int -> Char
int2mayuscula n = chr (ord 'A' + n)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    desplaza :: Int -> Char -> Char
-- tal que (desplaza n c) es el carácter obtenido desplazando n
-- caracteres el carácter c. Por ejemplo,
--    desplaza   3  'a'  ==  'd'
--    desplaza   3  'y'  ==  'b'
--    desplaza (-3) 'd'  ==  'a'
--    desplaza (-3) 'b'  ==  'y'
--    desplaza   3  'A'  ==  'D'
--    desplaza   3  'Y'  ==  'B'
--    desplaza (-3) 'D'  ==  'A'
--    desplaza (-3) 'B'  ==  'Y'
-- ---------------------------------------------------------------------

desplaza :: Int -> Char -> Char
desplaza n c
  | elem c ['a'..'z'] = int2minuscula ((minuscula2int c+n) `mod` 26)
  | elem c ['A'..'Z'] = int2mayuscula ((mayuscula2int c+n) `mod` 26)
  | otherwise         = c

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función
--    codifica :: Int -> String -> String
-- tal que (codifica n xs) es el resultado de codificar el texto xs con
-- un desplazamiento n. Por ejemplo,
--    λ> codifica   3  "En Todo La Medida"
--    "Hq Wrgr Od Phglgd"
--    λ> codifica (-3) "Hq Wrgr Od Phglgd"
--    "En Todo La Medida"
-- ---------------------------------------------------------------------

codifica :: Int -> String -> String
codifica n xs = [desplaza n x | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar con QuickCheck que para cualquier entero n y
-- cualquier cadena cs se tiene que (codifica (-n) (codifica n cs)) es
-- igual a cs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_codifica :: Int -> String -> Bool
prop_codifica n cs =
  codifica (-n) (codifica n cs) == cs

-- La comprobación es
--    λ> quickCheck prop_codifica
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    tabla :: [Float]
-- tal que tabla es la lista de la frecuencias de las letras en
-- castellano, Por ejemplo, la frecuencia de la 'a' es del 12.53%, la de
-- la 'b' es 1.42%.
-- ---------------------------------------------------------------------

tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01,
          0.70, 6.25, 0.44, 0.01,  4.97, 3.15, 6.71,
          8.68, 2.51, 0.88, 6.87,  7.98, 4.63, 3.93,
          0.90, 0.02, 0.22, 0.90,  0.52]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    porcentaje :: Int -> Int -> Float
-- tal que (porcentaje n m) es el porcentaje de n sobre m. Por ejemplo,
--    porcentaje 2 5  ==  40.0
-- ---------------------------------------------------------------------

porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n / fromIntegral m) * 100

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    letras :: String -> String
-- tal que (letras xs) es la cadena formada por las letras de la cadena
-- xs. Por ejemplo,
--    letras "Esto Es Una Prueba"  ==  "EstoEsUnaPrueba"
-- ---------------------------------------------------------------------

letras :: String -> String
letras xs = [x | x <- xs, elem x (['a'..'z']++['A'..'Z'])]

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. Definir la función
--    ocurrencias :: Eq a => a -> [a] -> Int
-- tal que (ocurrencias x xs) es el número de veces que ocurre el
-- elemento x en la lista xs. Por ejemplo,
--    ocurrencias 'a' "Salamanca"  ==  4
-- ---------------------------------------------------------------------

ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias x xs = length [x' | x' <- xs, x == x']

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Comprobar con QuickCheck si el número de ocurrencias
-- de un elemento x en una lista xs es igual que en su inversa.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ocurrencia_inv :: Int -> [Int] -> Bool
prop_ocurrencia_inv x xs =
  ocurrencias x xs == ocurrencias x (reverse xs)

-- La comprobación es
--    λ> quickCheck prop_ocurrencia_inv
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Comprobar con QuickCheck si el número de ocurrencias
-- de un elemento x en la concatenación de las listas xs e ys es igual a
-- la suma del número de ocurrencias de x en xs y en ys.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ocurrencia_conc :: Int -> [Int] -> [Int] -> Bool
prop_ocurrencia_conc x xs ys =
  ocurrencias x (xs++ys) == ocurrencias x xs + ocurrencias x ys

-- La comprobación es
--    λ> quickCheck prop_ocurrencia_conc
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    frecuencias :: String -> [Float]
-- tal que (frecuencias xs) es la frecuencia de cada una de las letras
-- de la cadena xs. Por ejemplo,
--    λ> frecuencias "En Todo La Medida"
--    [14.3,0,0,21.4,14.3,0,0,0,7.1,0,0,7.1,
--     7.1,7.1,14.3,0,0,0,0,7.1,0,0,0,0,0,0]
-- ---------------------------------------------------------------------

frecuencias :: String -> [Float]
frecuencias xs =
  [porcentaje (ocurrencias x xs') n | x <- ['a'..'z']]
  where xs' = [toLower x | x <- xs]
        n   = length (letras xs)

-- ---------------------------------------------------------------------
-- Ejercicio 13.1. Definir la función
--    chiCuad :: [Float] -> [Float] -> Float
-- tal que (chiCuad os es) es la medida chi cuadrado de las
-- distribuciones os y es. Por ejemplo,
--    chiCuad [3,5,6] [3,5,6]  ==  0.0
--    chiCuad [3,5,6] [5,6,3]  ==  3.9666667
-- ---------------------------------------------------------------------

chiCuad :: [Float] -> [Float] -> Float
chiCuad os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- ---------------------------------------------------------------------
-- Ejercicio 13.2, Comprobar con QuickCheck que para cualquier par de
-- listas xs e ys se verifica que (chiCuad xs ys) es 0 syss xs e ys son
-- iguales.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_chiCuad_1 :: [Float] -> [Float] -> Bool
prop_chiCuad_1 xs ys =
  (chiCuad xs ys == 0) == (xs == ys)

-- La comprobación es
--    λ> quickCheck prop_chiCuad_1
--    *** Failed! Falsifiable (after 2 tests and 2 shrinks):
--    [2.0]
--    []
-- En efecto,
--    λ> chiCuad [2] [] == 0
--    True
--    λ> [2] == []
--    False

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. A la vista de los contraejemplos del apartado
-- anterior, qué condición hay que añadir para que se verifique la
-- propiedad.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_chiCuad_2 :: [Float] -> [Float] -> Property
prop_chiCuad_2 xs ys =
  length xs == length ys ==> (chiCuad xs ys == 0) == (xs == ys)

-- La comprobación es
--    λ> quickCheck prop_chiCuad_2
--    *** Gave up! Passed only 47 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. A la vista del apartado anterior, el número de tests
-- que ha pasado puede ser menor que 100. Reescribir la propiedad de
-- forma que se verifique en los 100 tests.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_chiCuad_3 :: [Float] -> [Float] -> Bool
prop_chiCuad_3 xs ys =
  (chiCuad as bs == 0) == (as == bs)
  where n  = min (length xs) (length ys)
        as = take n xs
        bs = take n ys

-- La comprobación es
--    λ> quickCheck prop_chiCuad_3
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 14.1. Definir la función
--    rota :: Int -> [a] -> [a]
-- tal que (rota n xs) es la lista obtenida rotando n posiciones los
-- elementos de la lista xs. Por ejemplo,
--    rota  2 "manolo"              ==  "noloma"
--    rota 10 "manolo"              ==  "lomano"
--    [rota n "abc" | n <- [0..5]]  ==  ["abc","bca","cab","abc","bca","cab"]
-- ---------------------------------------------------------------------

rota :: Int -> [a] -> [a]
rota _ [] = []
rota n xs = drop m xs ++ take m xs
  where m = n `mod` length xs

-- ---------------------------------------------------------------------
-- Ejercicio 14.2. Comprobar con QuickCkeck si para cualquier lista xs
-- si se rota n veces y el resultado se rota m veces se obtiene lo mismo
-- que rotando xs (n+m) veces, donde n y m son números no nulos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_rota :: Int -> Int -> [Int] -> Property
prop_rota n m xs =
  n /= 0 && m /= 0 ==> rota m (rota n xs) == rota (n+m) xs

-- La comprobación es
--    λ> quickCheck prop_rota
--    *** Failed! Falsifiable (after 79 tests and 57 shrinks):
--    877320888
--    1270162760
--    [0,0,1]

-- El error se debe a que el número 877320888 + 1270162760 es demasiado
-- grande. Acotando los números de las rotaciones se tiene

prop_rota2 :: Int -> Int -> [Int] -> Property
prop_rota2 n m xs =
  abs n < 2^30 && abs m < 2^30 ==>
  rota m (rota n xs) == rota (n+m) xs

-- ---------------------------------------------------------------------
-- Ejercicio 15.1. Definir la función
--    descifra :: String -> String
-- tal que (descifra xs) es la cadena obtenida descodificando la cadena
-- xs por el anti-desplazamiento que produce una distribución de letras
-- con la menor deviación chi cuadrado respecto de la tabla de
-- distribución de las letras en castellano. Por ejemplo,
--    λ> codifica 5 "Todo Para Nada"
--    "Ytit Ufwf Sfif"
--    λ> descifra "Ytit Ufwf Sfif"
--    "Todo Para Nada"
-- ---------------------------------------------------------------------

descifra :: String -> String
descifra xs =  codifica (-factor) xs
  where factor = head (posiciones (minimum tabChi) tabChi)
        tabChi = [chiCuad (rota n tabla') tabla | n <- [0..25]]
        tabla' = frecuencias xs

posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs =
  [i | (x',i) <- zip xs [0..], x == x']
