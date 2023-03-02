-- El_TAD_de_las_colas.hs
-- El tipo abstracto de datos de las colas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir funciones sobre
-- el TAD de las colas, utilizando las implementaciones estudiadas en el
-- tema 15 transparencias se encuentran en
--    https://jaalonso.github.io/cursos/i1m/temas/tema-15.html
--
-- Para realizar los ejercicios hay que tener instalada la librería de
-- I1M. Para instalarla basta ejecutar en una consola
--    cabal update
--    cabal install I1M
--
-- Otra forma es descargar las implementaciones de las implementaciones
-- de las colas:
-- + ColaConListas.hs    que está en https://bit.ly/2Zk0rgZ
-- + ColaConDosListas.hs que está en https://bit.ly/2XPr7pB

{-# OPTIONS_GHC -fno-warn-unused-matches
                -fno-warn-unused-imports
                -fno-warn-orphans
#-}

module El_TAD_de_las_colas where

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

-- Hay que elegir una implementación del TAD colas:
import I1M.Cola
-- import ColaConListas
-- import ColaConDosListas

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Nota. A lo largo de la relación de ejercicios usaremos los siguientes
-- ejemplos de colas:
ejCola1, ejCola2, ejCola3, ejCola4, ejCola5, ejCola6 :: Cola Int
ejCola1 = foldr inserta vacia [1..20]
ejCola2 = foldr inserta vacia [2,5..18]
ejCola3 = foldr inserta vacia [3..10]
ejCola4 = foldr inserta vacia [4,-1,7,3,8,10,0,3,3,4]
ejCola5 = foldr inserta vacia [15..20]
ejCola6 = foldr inserta vacia (reverse [1..20])
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
--    ultimoCola :: Cola a -> a
-- tal que (ultimoCola c) es el último elemento de la cola c. Por
-- ejemplo:
--    ultimoCola ejCola4 == 4
--    ultimoCola ejCola5 == 15
-- ---------------------------------------------------------------------

ultimoCola :: Cola a -> a
ultimoCola c
  | esVacia c  = error "cola vacia"
  | esVacia rc = pc
  | otherwise  = ultimoCola rc
  where pc = primero c
        rc = resto c

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
--    longitudCola :: Cola a -> Int
-- tal que (longitudCola c) es el número de elementos de la cola c. Por
-- ejemplo,
--    longitudCola ejCola2 == 6
-- ---------------------------------------------------------------------

longitudCola :: Cola a -> Int
longitudCola c
  | esVacia c = 0
  | otherwise = 1 + longitudCola rc
  where rc = resto c

-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función
--    todosVerifican :: (a -> Bool) -> Cola a -> Bool
-- tal que (todosVerifican p c) se verifica si todos los elementos de la
-- cola c cumplen la propiedad p. Por ejemplo,
--    todosVerifican (>0) ejCola1 == True
--    todosVerifican (>0) ejCola4 == False
-- ---------------------------------------------------------------------

todosVerifican :: (a -> Bool) -> Cola a -> Bool
todosVerifican p c
  | esVacia c = True
  | otherwise = p pc && todosVerifican p rc
  where pc = primero c
        rc = resto c

-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir la función
--    algunoVerifica :: (a -> Bool) -> Cola a -> Bool
-- tal que (algunoVerifica p c) se verifica si algún elemento de la cola
-- c cumple la propiedad p. Por ejemplo,
--    algunoVerifica (<0) ejCola1 == False
--    algunoVerifica (<0) ejCola4 == True
-- ---------------------------------------------------------------------

algunoVerifica :: (a -> Bool) -> Cola a -> Bool
algunoVerifica p c
  | esVacia c = False
  | otherwise = p pc || algunoVerifica p rc
  where pc = primero c
        rc = resto c

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir la función
--    extiendeCola :: Cola a -> Cola a -> Cola a
-- tal que (extiendeCola c1 c2) es la cola que resulta de poner los
-- elementos de c2 a la cola de c1. Por ejemplo,
--    extiendeCola ejCola2 ejCola3 == C [17,14,11,8,5,2,10,9,8,7,6,5,4,3]
-- ---------------------------------------------------------------------

extiendeCola :: Cola a -> Cola a -> Cola a
extiendeCola c1 c2
  | esVacia c2 = c1
  | otherwise  = extiendeCola (inserta pc2 c1) rq2
  where pc2 = primero c2
        rq2 = resto c2

-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir la función
--    intercalaColas :: Cola a -> Cola a -> Cola a
-- tal que (intercalaColas c1 c2) es la cola formada por los elementos de
-- c1 y c2 colocados en una cola, de forma alternativa, empezando por
-- los elementos de c1. Por ejemplo,
--    intercalaColas ejCola2 ejCola4 == C [17,4,14,3,11,3,8,0,5,10,2,8,3,7,-1,4]
-- ---------------------------------------------------------------------

intercalaColas :: Cola a -> Cola a -> Cola a
intercalaColas c1 c2 = aux c1 c2 vacia
  where aux d1 d2 c
            | esVacia d1 = extiendeCola c d2
            | esVacia d2 = extiendeCola c d1
            | otherwise  = aux rd1 rd2 (inserta pd2 (inserta pd1 c))
          where pd1 = primero d1
                rd1 = resto d1
                pd2 = primero d2
                rd2 = resto d2

-- ---------------------------------------------------------------------
-- Ejercicio 7: Definir la función
--    agrupaColas :: [Cola a] -> Cola a
-- tal que (agrupaColas [c1,c2,c3,...,cn]) es la cola formada mezclando
-- las colas de la lista como sigue: mezcla c1 con c2, el resultado con
-- c3, el resultado con c4, y así sucesivamente. Por ejemplo,
--    λ> agrupaColas [ejCola3,ejCola3,ejCola4]
--    C [10,4,10,3,9,3,9,0,8,10,8,8,7,3,7,7,6,-1,6,4,5,5,4,4,3,3]
-- ---------------------------------------------------------------------

agrupaColas :: [Cola a] -> Cola a
agrupaColas []            = vacia
agrupaColas [c]           = c
agrupaColas (c1:c2:colas) = agrupaColas (intercalaColas c1 c2 : colas)

-- 2ª solución
agrupaColas2 :: [Cola a] -> Cola a
agrupaColas2 = foldl intercalaColas vacia

-- ---------------------------------------------------------------------
-- Ejercicio 8: Definir la función
--    perteneceCola :: Eq a => a -> Cola a -> Bool
-- tal que (perteneceCola x c) se verifica si x es un elemento de la
-- cola c. Por ejemplo,
--    perteneceCola 7 ejCola1  == True
--    perteneceCola 70 ejCola1 == False
-- ---------------------------------------------------------------------

perteneceCola :: Eq a => a -> Cola a -> Bool
perteneceCola x c
  | esVacia c  = False
  | otherwise  = pc == x || perteneceCola x rc
  where pc = primero c
        rc = resto c

-- ---------------------------------------------------------------------
-- Ejercicio 9: Definir la función
--    contenidaCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (contenidaCola c1 c2) se verifica si todos los elementos de
-- c1 son elementos de c2. Por ejemplo,
--    contenidaCola ejCola2 ejCola1 == True
--    contenidaCola ejCola1 ejCola2 == False
-- ---------------------------------------------------------------------

contenidaCola :: Eq a => Cola a -> Cola a -> Bool
contenidaCola c1 c2
  | esVacia c1 = True
  | esVacia c2 = False
  | otherwise  = perteneceCola pc1 c2 && contenidaCola rc1 c2
  where pc1 = primero c1
        rc1 = resto c1

-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir la función
--    prefijoCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (prefijoCola c1 c2) se verifica si la cola c1 es un prefijo
-- de la cola c2. Por ejemplo,
--    prefijoCola ejCola3 ejCola2 == False
--    prefijoCola ejCola5 ejCola1 == True
-- ---------------------------------------------------------------------

prefijoCola :: Eq a => Cola a -> Cola a -> Bool
prefijoCola c1 c2
  | esVacia c1 = True
  | esVacia c2 = False
  | otherwise  = pc1 == pc2 && prefijoCola rc1 rc2
  where pc1 = primero c1
        rc1 = resto c1
        pc2 = primero c2
        rc2 = resto c2

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir la función
--    subCola :: Eq a => Cola a -> Cola a -> Bool
-- tal que (subCola c1 c2) se verifica si c1 es una subcola de c2. Por
-- ejemplo,
--    subCola ejCola2 ejCola1 == False
--    subCola ejCola3 ejCola1 == True
-- ---------------------------------------------------------------------

subCola :: Eq a => Cola a -> Cola a -> Bool
subCola c1 c2
  | esVacia c1 = True
  | esVacia c2 = False
  | pc1 == pc2 = prefijoCola rc1 rc2 || subCola c1 rc2
  | otherwise  = subCola c1 rc2
  where pc1 = primero c1
        rc1 = resto c1
        pc2 = primero c2
        rc2 = resto c2

-- ---------------------------------------------------------------------
-- Ejercicio 12: Definir la función
--    ordenadaCola :: Ord a => Cola a -> Bool
-- tal que (ordenadaCola c) se verifica si los elementos de la cola c
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaCola ejCola6 == True
--    ordenadaCola ejCola4 == False
-- ---------------------------------------------------------------------

ordenadaCola :: Ord a => Cola a -> Bool
ordenadaCola c
  | esVacia c = True
  | esVacia rc = True
  | otherwise = pc <= prc && ordenadaCola rc
  where pc  = primero c
        rc  = resto c
        prc = primero rc

-- ---------------------------------------------------------------------
-- Ejercicio 13.1: Definir una función
--    lista2Cola :: [a] -> Cola a
-- tal que (lista2Cola xs) es una cola formada por los elementos de xs.
-- Por ejemplo,
--    lista2Cola [1..6] == C [1,2,3,4,5,6]
-- ---------------------------------------------------------------------

lista2Cola :: [a] -> Cola a
lista2Cola xs = foldr inserta vacia (reverse xs)

-- ---------------------------------------------------------------------
-- Ejercicio 13.2: Definir una función
--    cola2Lista :: Cola a -> [a]
-- tal que (cola2Lista c) es la lista formada por los elementos de p.
-- Por ejemplo,
--    cola2Lista ejCola2 == [17,14,11,8,5,2]
-- ---------------------------------------------------------------------

cola2Lista :: Cola a -> [a]
cola2Lista c
  | esVacia c = []
  | otherwise = pc : cola2Lista rc
  where pc = primero c
        rc = resto c

-- ---------------------------------------------------------------------
-- Ejercicio 13.3. Comprobar con QuickCheck que la función cola2Lista es
-- la inversa de  lista2Cola, y recíprocamente.
-- ---------------------------------------------------------------------

prop_cola2Lista :: Cola Int -> Bool
prop_cola2Lista c =
  lista2Cola (cola2Lista c) == c

-- λ> quickCheck prop_cola2Lista
-- +++ OK, passed 100 tests.

prop_lista2Cola :: [Int] -> Bool
prop_lista2Cola xs =
    cola2Lista (lista2Cola xs) == xs

-- λ> quickCheck prop_lista2Cola
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 14: Definir la función
--    maxCola :: Ord a => Cola a -> a
-- tal que (maxCola c) es el mayor de los elementos de la cola c. Por
-- ejemplo,
--    maxCola ejCola4 == 10
-- ---------------------------------------------------------------------

maxCola :: Ord a => Cola a -> a
maxCola c
  | esVacia c = error "cola vacia"
  | esVacia rc = pc
  | otherwise = max pc (maxCola rc)
  where pc = primero c
        rc = resto c

prop_maxCola :: Cola Int -> Property
prop_maxCola c =
  not (esVacia c) ==>
  maxCola c == maximum (cola2Lista c)

-- λ> quickCheck prop_maxCola
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Generador de colas                                          --
-- ---------------------------------------------------------------------

-- genCola es un generador de colas de enteros. Por ejemplo,
--    λ> sample genCola
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([],[])
--    C ([7,8,4,3,7],[5,3,3])
--    C ([],[])
--    C ([1],[13])
--    C ([18,28],[12,21,28,28,3,18,14])
--    C ([47],[64,45,7])
--    C ([8],[])
--    C ([42,112,178,175,107],[])
genCola :: (Num a, Arbitrary a) => Gen (Cola a)
genCola = frequency [(1, return vacia),
                     (30, do n <- choose (10,100)
                             xs <- vectorOf n arbitrary
                             return (creaCola xs))]
  where creaCola = foldr inserta vacia

-- El tipo cola es una instancia del arbitrario.
instance (Arbitrary a, Num a) => Arbitrary (Cola a) where
  arbitrary = genCola
