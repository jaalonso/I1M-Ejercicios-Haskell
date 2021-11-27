-- Codificacion_por_longitud.hs
-- Codificación por longitud.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Codificacion_por_longitud where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- La codificación por longitud, o comprensión RLE (del inglés,
-- "Run-length encoding"), es una compresión de datos en la que
-- secuencias de datos con el mismo valor consecutivas son almacenadas
-- como un único valor más su recuento. Por ejemplo, la cadena
--    BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBBBBBBBNBBBBBBBBBBBBBB
-- se codifica por
--    12B1N12B3N24B1N14B
-- Interpretado esto como 12 letras B, 1 letra N , 12 letras B, 3 letras
-- N, etc.
--
-- En los siguientes ejercicios se definirán funciones para codificar y
-- descodificar por longitud.

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.Char
import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una lista se puede comprimir indicando el número de
-- veces consecutivas que aparece cada elemento. Por ejemplo, la lista
-- comprimida de [1,1,7,7,7,5,5,7,7,7,7] es [(2,1),(3,7),(2,5),(4,7)],
-- indicando que comienza con dos 1, seguido de tres 7, dos 5 y cuatro
-- 7.
--
-- Definir la función
--    comprimida :: Eq a => [a] -> [(Int,a)]
-- tal que (comprimida xs) es la lista obtenida al comprimir por
-- longitud la lista xs. Por ejemplo,
--    λ> comprimida [1,1,7,7,7,5,5,7,7,7,7]
--    [(2,1),(3,7),(2,5),(4,7)]
--    λ> comprimida "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBBBBBBBBBB"
--    [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
--    λ> comprimida []
--    []
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
comprimida :: Eq a => [a] -> [(Int,a)]
comprimida xs = aux xs 1
  where aux (x:y:zs) n | x == y    = aux (y:zs) (n+1)
                       | otherwise = (n,x) : aux (y:zs) 1
        aux [x]      n             = [(n,x)]
        aux []       _             = []

-- 2ª definición (por recursión usando takeWhile):
comprimida2 :: Eq a => [a] -> [(Int,a)]
comprimida2 [] = []
comprimida2 (x:xs) =
  (1 + length (takeWhile (==x) xs),x) : comprimida2 (dropWhile (==x) xs)

-- 3ª definición (por comprensión usando group):
comprimida3 :: Eq a => [a] -> [(Int,a)]
comprimida3 xs = [(length ys, head ys) | ys <- group xs]

-- 4ª definición (usando map y group):
comprimida4 :: Eq a => [a] -> [(Int,a)]
comprimida4 = map (\xs -> (length xs, head xs)) . group

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    expandida :: [(Int,a)] -> [a]
-- tal que (expandida ps) es la lista expandida correspondiente a ps (es
-- decir, es la lista xs tal que la comprimida de xs es ps). Por
-- ejemplo,
--    expandida [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
expandida :: [(Int,a)] -> [a]
expandida ps = concat [replicate k x | (k,x) <- ps]

-- 2ª definición (por concatMap)
expandida2 :: [(Int,a)] -> [a]
expandida2 = concatMap (\(k,x) -> replicate k x)

-- 3ª definición (por recursión)
expandida3 :: [(Int,a)] -> [a]
expandida3 []         = []
expandida3 ((n,x):ps) = replicate n x ++ expandida3 ps

-- 4ª definición
expandida4 :: [(Int,a)] -> [a]
expandida4 = concatMap (uncurry replicate)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Comprobar con QuickCheck que dada una lista de enteros,
-- si se la comprime y después se expande se obtiene la lista inicial.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_expandida_comprimida :: [Int] -> Bool
prop_expandida_comprimida xs = expandida (comprimida xs) == xs

-- La comprobación es
--    λ> quickCheck prop_expandida_comprimida
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Comprobar con QuickCheck que dada una lista de pares
-- de enteros, si se la expande y después se comprime se obtiene la
-- lista inicial.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_comprimida_expandida :: [(Int,Int)] -> Bool
prop_comprimida_expandida xs = expandida (comprimida xs) == xs

-- La comprobación es
--    λ> quickCheck prop_comprimida_expandida
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    listaAcadena :: [(Int,Char)] -> String
-- tal que (listaAcadena xs) es la cadena correspondiente a la lista de
-- pares de xs. Por ejemplo,
--    λ> listaAcadena [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')]
--    "12B1N12B3N19B"
-- ---------------------------------------------------------------------

listaAcadena :: [(Int,Char)] -> String
listaAcadena xs = concat [show n ++ [c] | (n,c) <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    cadenaComprimida :: String -> String
-- tal que (cadenaComprimida cs) es la cadena obtenida comprimiendo por
-- longitud la cadena cs. Por ejemplo,
--    λ> cadenaComprimida "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
--    "12B1N12B3N10B3N"
-- ---------------------------------------------------------------------

cadenaComprimida :: String -> String
cadenaComprimida = listaAcadena . comprimida

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    cadenaAlista :: String -> [(Int,Char)]
-- tal que (cadenaAlista cs) es la lista de pares correspondientes a la
-- cadena cs. Por ejemplo,
--    λ> cadenaAlista "12B1N12B3N10B3N"
--    [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(10,'B'),(3,'N')]
-- ---------------------------------------------------------------------

cadenaAlista :: String -> [(Int,Char)]
cadenaAlista [] = []
cadenaAlista cs = (read ns,x) : cadenaAlista xs
  where (ns,x:xs) = span isNumber cs

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    cadenaExpandida :: String -> String
-- tal que (cadenaExpandida cs) es la cadena expandida correspondiente a
-- cs (es decir, es la cadena xs que al comprimirse por longitud da cs).
-- Por ejemplo,
--    λ> cadenaExpandida "12B1N12B3N10B3N"
--    "BBBBBBBBBBBBNBBBBBBBBBBBBNNNBBBBBBBBBBNNN"
-- ---------------------------------------------------------------------

cadenaExpandida :: String -> String
cadenaExpandida = expandida . cadenaAlista
