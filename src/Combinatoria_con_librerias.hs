-- Combinatoria_con_librerias.hs
-- Combinatoria con librerías.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Combinatoria_con_librerias where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es redefinir algunos ejercicios de la
-- relación anterior usando la librería de combinatoria
--    Math.Combinat.Sets   https://bit.ly/3DEcogL

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List (permutations)
import Math.Combinat.Sets

-- ---------------------------------------------------------------------
-- § Subconjuntos
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    subconjuntos :: [a] -> [[a]]
-- tal que (subconjuntos xs) es la lista de las subconjuntos de la lista
-- xs. Por ejemplo,
--    λ> subconjuntos [2,3,4]
--    [[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4]]
--    λ> subconjuntos [1,2,3,4]
--    [[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4],
--     [1],[1,4],[1,3],[1,3,4],[1,2],[1,2,4],[1,2,3],[1,2,3,4]]
-- ---------------------------------------------------------------------

subconjuntos :: [a] -> [[a]]
subconjuntos = sublists

-- ---------------------------------------------------------------------
-- § Permutaciones
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    permutaciones :: [a] -> [[a]]
-- tal que (permutaciones xs) es la lista de las permutaciones de la
-- lista xs. Por ejemplo,
--    permutaciones "bc"   ==  ["bc","cb"]
--    permutaciones "abc"  ==  ["abc","bac","cba","bca","cab","acb"]
-- ---------------------------------------------------------------------

permutaciones :: [a] -> [[a]]
permutaciones = permutations

-- ---------------------------------------------------------------------
-- § Combinaciones
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    combinaciones :: Int -> [a] -> [[a]]
-- tal que (combinaciones k xs) es la lista de las combinaciones de
-- orden k de los elementos de la lista xs. Por ejemplo,
--    λ> combinaciones 2 "bcde"
--    ["bc","bd","be","cd","ce","de"]
--    λ> combinaciones 3 "bcde"
--    ["bcd","bce","bde","cde"]
--    λ> combinaciones 3 "abcde"
--    ["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]
-- ---------------------------------------------------------------------

combinaciones :: Int -> [a] -> [[a]]
combinaciones = choose

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir, usando combinacionesN, la función
--    numeroCombinaciones :: Int -> Int -> Integer
-- tal que (numeroCombinaciones n k) es el número de combinaciones de
-- orden k de un conjunto con n elementos. Por ejemplo,
--    numeroCombinaciones 4 2  ==  6
--    numeroCombinaciones 4 3  ==  4
-- ---------------------------------------------------------------------

numeroCombinaciones :: Int -> Int -> Integer
numeroCombinaciones n k = countKSublists k n

-- ---------------------------------------------------------------------
-- § Combinaciones con repetición
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    combinacionesR :: Int -> [a] -> [[a]]
-- tal que (combinacionesR k xs) es la lista de las combinaciones orden
-- k de los elementos de xs con repeticiones. Por ejemplo,
--    λ> combinacionesR 2 "abc"
--    ["aa","ba","ab","bb"]
--    λ> combinacionesR 3 "bc"
--    ["bbb","bbc","bcc","ccc"]
--    λ> combinacionesR 3 "abc"
--    ["aaa","baa","aba","bba","aab","bab","abb","bbb"]
-- ---------------------------------------------------------------------

combinacionesR :: Int -> [a] -> [[a]]
combinacionesR = combine

-- ---------------------------------------------------------------------
-- § Variaciones
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--    variaciones :: Int -> [a] -> [[a]]
-- tal que (variaciones n xs) es la lista de las variaciones n-arias
-- de la lista xs. Por ejemplo,
--    variaciones 2 "abc"  ==  ["ab","ba","ac","ca","bc","cb"]
-- ---------------------------------------------------------------------

variaciones :: Int -> [a] -> [[a]]
variaciones k xs = concatMap permutations (choose k xs)

-- ---------------------------------------------------------------------
-- § Variaciones con repetición
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    variacionesR :: Int -> [a] -> [[a]]
-- tal que (variacionesR k xs) es la lista de las variaciones de orden
-- k de los elementos de xs con repeticiones. Por ejemplo,
--    λ> variacionesR 1 "ab"
--    ["a","b"]
--    λ> variacionesR 2 "ab"
--    ["aa","ab","ba","bb"]
--    λ> variacionesR 3 "ab"
--    ["aaa","aab","aba","abb","baa","bab","bba","bbb"]
-- ---------------------------------------------------------------------

variacionesR :: Int -> [a] -> [[a]]
variacionesR = tuplesFromList
