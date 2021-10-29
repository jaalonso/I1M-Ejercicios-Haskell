-- Operaciones_con_conjuntos.hs
-- Operaciones con conjuntos.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir operaciones
-- entre conjuntos, representados mediante listas ordenadas sin
-- repeticiones, explicado en el tema 17 cuyas transparencias se
-- encuentran en
--    https://jaalonso.github.io/cursos/i1m/temas/tema-17.html

{-# LANGUAGE FlexibleInstances #-}

module Operaciones_con_conjuntos where

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- § Representación de conjuntos y operaciones básicas                --
-- ---------------------------------------------------------------------

-- Los conjuntos como listas ordenadas sin repeticiones.
newtype Conj a = Cj [a]
  deriving Eq

-- Ejemplo de conjunto:
--    λ> ejConj1
--    Cj [0,1,2,3,5,7,9]
ejConj1 :: Conj Int
ejConj1 = foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]

-- Procedimiento de escritura de los conjuntos.
instance Show a => Show (Conj a) where
  show = escribeConj

-- (escribeConj c) es la cadena correspondiente al conjunto c. Por
-- ejemplo,
--    λ> ejConj1
--    Cj [0,1,2,3,5,7,9]
--    λ> escribeConj ejConj1
--    "{0,1,2,3,5,7,9}"
escribeConj :: Show a => Conj a -> String
escribeConj (Cj [])     = "{}"
escribeConj (Cj (x:xs)) = "{" ++ show x ++ aux xs
  where aux []     = "}"
        aux (y:ys) = "," ++ show y ++ aux ys

-- vacio es el conjunto vacío. Por ejemplo,
--    λ> vacio
--    Cj []
--    λ> escribeConj vacio
--    "{}"
vacio :: Conj a
vacio = Cj []

-- (esVacio c) se verifica si c es el conjunto vacío. Por ejemplo,
--    esVacio ejConj1     ==  False
--    esVacio vacio  ==  True
esVacio :: Conj a -> Bool
esVacio (Cj xs) = null xs

-- (pertenece x c) se verifica si x pertenece al conjunto c. Por ejemplo,
--    λ> ejConj1
--    Cj [0,1,2,3,5,7,9]
--    λ> pertenece 3 ejConj1
--    True
--    λ> pertenece 4 ejConj1
--    False
pertenece :: Ord a => a -> Conj a -> Bool
pertenece x (Cj s) = x `elem` takeWhile (<= x) s

-- (inserta x c) es el conjunto obtenido añadiendo el elemento x al
-- conjunto c. Por ejemplo,
--    λ> ejConj1
--    Cj [0,1,2,3,5,7,9]
--    λ> inserta 5 ejConj1
--    Cj [0,1,2,3,5,7,9]
--    λ> inserta 4 ejConj1
--    Cj [0,1,2,3,4,5,7,9]
inserta :: Ord a => a -> Conj a -> Conj a
inserta x (Cj ys) = Cj (insertaL x ys)

-- (insertaL x ys) es la lista obtenida añadiendo el elemento x a la
-- lista ordenada ys. Por ejemplo,
--    λ> insertaL 5 [0,1,2,3,5,7,9]
--    [0,1,2,3,5,7,9]
--    λ> insertaL 4 [0,1,2,3,5,7,9]
--    [0,1,2,3,4,5,7,9]
insertaL :: Ord a => a -> [a] -> [a]
insertaL x []                  = [x]
insertaL x (y:ys) | x > y      = y : insertaL x ys
                  | x < y      = x : y : ys
                  | otherwise  = y : ys

-- (elimina x c) es el conjunto obtenido eliminando el elemento x
-- del conjunto c. Por ejemplo,
--    λ> ejConj1
--    Cj [0,1,2,3,5,7,9]
--    λ> elimina 3 ejConj1
--    Cj [0,1,2,5,7,9]
--    λ> elimina 4 ejConj1
--    Cj [0,1,2,3,5,7,9]
elimina :: Ord a => a -> Conj a -> Conj a
elimina x (Cj ys) = Cj (eliminaL x ys)

-- (eliminaL x ys) es la lista obtenida eliminando el elemento x
-- de la lista ordenada ys. Por ejemplo,
--    λ> eliminaL 3 [0,1,2,3,5,7,9]
--    [0,1,2,5,7,9]
--    λ> eliminaL 4 [0,1,2,3,5,7,9]
--    [0,1,2,3,5,7,9]
eliminaL :: Ord a => a -> [a] -> [a]
eliminaL _ []                 = []
eliminaL x (y:ys) | x > y     = y : eliminaL x ys
                  | x < y     = y : ys
                  | otherwise = ys

-- Ejemplos de conjunto:
ejConj2, ejConj3, ejConj4 :: Conj Int
ejConj2 = foldr inserta vacio [2,6,8,6,1,2,1,9,6]
ejConj3 = Cj [2..100000]
ejConj4 = Cj [1..100000]

-- ---------------------------------------------------------------------
-- § Ejercicios                                                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    subconjunto :: Ord a => Conj a -> Conj a -> Bool
-- tal que (subconjunto c1 c2) se verifica si todos los elementos de c1
-- pertenecen a c2. Por ejemplo,
--    subconjunto (Cj [2..100000]) (Cj [1..100000]) == True
--    subconjunto (Cj [1..100000]) (Cj [2..100000]) == False
-- ---------------------------------------------------------------------

-- 1ª definición
subconjunto1 :: Ord a => Conj a -> Conj a -> Bool
subconjunto1 (Cj xs) (Cj ys) = sublista xs ys
  where sublista [] _      = True
        sublista (z:zs) us = elem z ys && sublista zs us

-- 2ª definición
subconjunto2 :: Ord a => Conj a -> Conj a -> Bool
subconjunto2 (Cj xs) c =
  and [pertenece x c | x <- xs]

-- 3ª definición
subconjunto3 :: Ord a => Conj a -> Conj a -> Bool
subconjunto3 (Cj xs) (Cj ys) = sublista xs ys
  where
    sublista [] _               = True
    sublista _ []               = False
    sublista (x:xs') ys'@(y:zs) = x >= y && elem x ys' && sublista xs' zs

-- Comparación de la eficiencia:
--    λ> subconjunto1 (Cj [2..100000]) (Cj [1..1000000])
--      C-c C-cInterrupted.
--    λ> subconjunto2 (Cj [2..100000]) (Cj [1..1000000])
--      C-c C-cInterrupted.
--    λ> subconjunto3 (Cj [2..100000]) (Cj [1..1000000])
--    True
--    (0.52 secs, 26097076 bytes)
--    λ> subconjunto4 (Cj [2..100000]) (Cj [1..1000000])
--    True
--    (0.66 secs, 32236700 bytes)
--    λ> subconjunto1 (Cj [2..100000]) (Cj [1..10000])
--    False
--    (0.54 secs, 3679024 bytes)
--    λ> subconjunto2 (Cj [2..100000]) (Cj [1..10000])
--    False
--    (38.19 secs, 1415562032 bytes)
--    λ> subconjunto3 (Cj [2..100000]) (Cj [1..10000])
--    False
--    (0.08 secs, 3201112 bytes)
--    λ> subconjunto4 (Cj [2..100000]) (Cj [1..10000])
--    False
--    (0.09 secs, 3708988 bytes)

-- En lo que sigue, se usará la 3ª definición:
subconjunto :: Ord a => Conj a -> Conj a -> Bool
subconjunto = subconjunto3

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    subconjuntoPropio :: Ord a => Conj a -> Conj a -> Bool
-- tal (subconjuntoPropio c1 c2) se verifica si c1 es un subconjunto
-- propio de c2. Por ejemplo,
--   subconjuntoPropio (Cj [2..5]) (Cj [1..7]) == True
--   subconjuntoPropio (Cj [2..5]) (Cj [1..4]) == False
--   subconjuntoPropio (Cj [2..5]) (Cj [2..5]) == False
-- ---------------------------------------------------------------------

subconjuntoPropio :: Ord a => Conj a -> Conj a -> Bool
subconjuntoPropio c1 c2 =
  subconjunto c1 c2 && c1 /= c2

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    unitario :: Ord a => a -> Conj a
-- tal que (unitario x) es el conjunto {x}. Por ejemplo,
--   unitario 5 == {5}
-- ---------------------------------------------------------------------

unitario :: Ord a => a -> Conj a
unitario x = inserta x vacio

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    cardinal :: Conj a -> Int
-- tal que (cardinal c) es el número de elementos del conjunto c. Por
-- ejemplo,
--    cardinal ejConj1 == 7
--    cardinal ejConj2 == 5
-- ---------------------------------------------------------------------

cardinal :: Conj a -> Int
cardinal (Cj xs) = length xs

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    union :: Ord a => Conj a -> Conj a -> Conj a
-- tal (union c1 c2) es la unión de ambos conjuntos. Por ejemplo,
--    union ejConj1 ejConj2             == {0,1,2,3,5,6,7,8,9}
--    cardinal (union2 ejConj3 ejConj4) == 100000
-- ---------------------------------------------------------------------

-- 1ª definición:
union1 :: Ord a => Conj a -> Conj a -> Conj a
union1 (Cj xs) (Cj ys) = foldr inserta (Cj ys) xs

-- Otra definión es
union2 :: Ord a => Conj a -> Conj a -> Conj a
union2 (Cj xs) (Cj ys) = Cj (unionL xs ys)
  where
    unionL [] ys' = ys'
    unionL xs' [] = xs'
    unionL (x:xs') (y:ys')
      | x < y  = x : unionL xs' (y:ys')
      | x == y = x : unionL xs' ys'
      | x > y  = y : unionL (x:xs') ys'
    unionL _ _ = error "Imposible"

-- Comparación de eficiencia
--    λ> :set +s
--    λ> let c = Cj [1..1000]
--    λ> cardinal (union1 c c)
--    1000
--    (1.04 secs, 56914332 bytes)
--    λ> cardinal (union2 c c)
--    1000
--    (0.01 secs, 549596 bytes)

-- En lo que sigue se usará la 2ª definición
union :: Ord a => Conj a -> Conj a -> Conj a
union = union2

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    unionG:: Ord a => [Conj a] -> Conj a
-- tal (unionG cs) calcule la unión de la lista de conjuntos cd. Por
-- ejemplo,
--    unionG [ejConj1, ejConj2] == {0,1,2,3,5,6,7,8,9}
-- ---------------------------------------------------------------------

unionG :: Ord a => [Conj a] -> Conj a
unionG []          = vacio
unionG (Cj xs:css) = Cj xs `union` unionG css

-- Se puede definir por plegados
unionG2 :: Ord a => [Conj a] -> Conj a
unionG2 = foldr union vacio

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    interseccion :: Eq a => Conj a -> Conj a -> Conj a
-- tal que (interseccion c1 c2) es la intersección de los conjuntos c1 y
-- c2. Por ejemplo,
--    interseccion (Cj [1..7]) (Cj [4..9])    == {4,5,6,7}
--    interseccion (Cj [2..1000000]) (Cj [1]) == {}
-- ---------------------------------------------------------------------

-- 1ª definición
interseccion1 :: Eq a => Conj a -> Conj a -> Conj a
interseccion1 (Cj xs) (Cj ys) = Cj [x | x <- xs, x `elem` ys]

-- 2ª definición
interseccion2 :: Ord a => Conj a -> Conj a -> Conj a
interseccion2 (Cj xs) (Cj ys) = Cj (interseccionL xs ys)
  where
    interseccionL l1@(x:xs') l2@(y:ys')
      | x > y   = interseccionL l1 ys'
      | x == y  = x : interseccionL xs' ys'
      | x < y   = interseccionL xs' l2
    interseccionL _ _ = []

-- La comparación de eficiencia es
-- λ> interseccion1 (Cj [2..1000000]) (Cj [1])
-- {}
-- (0.32 secs, 80396188 bytes)
-- λ> interseccion2 (Cj [2..1000000]) (Cj [1])
-- {}
-- (0.00 secs, 2108848 bytes)

-- En lo que sigue se usa la 2ª definición:
interseccion :: Ord a => Conj a -> Conj a -> Conj a
interseccion = interseccion2

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    interseccionG:: Ord a => [Conj a] -> Conj a
-- tal que (interseccionG cs) es la intersección de la lista de
-- conjuntos cs. Por ejemplo,
--    interseccionG [ejConj1, ejConj2] == {1,2,9}
-- ---------------------------------------------------------------------

interseccionG :: Ord a => [Conj a] -> Conj a
interseccionG [c]      = c
interseccionG (cs:css) = interseccion cs (interseccionG css)
interseccionG []       = error "Imposible"

-- Se puede definir por plegado
interseccionG2 :: Ord a => [Conj a] -> Conj a
interseccionG2 = foldr1 interseccion

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    disjuntos :: Ord a => Conj a -> Conj a -> Bool
-- tal que (disjuntos c1 c2) se verifica si los conjuntos c1 y c2 son
-- disjuntos. Por ejemplo,
--   disjuntos (Cj [2..5]) (Cj [6..9]) == True
--   disjuntos (Cj [2..5]) (Cj [1..9]) == False
-- ---------------------------------------------------------------------

disjuntos :: Ord a => Conj a -> Conj a -> Bool
disjuntos c1 c2 = esVacio (interseccion c1 c2)

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    diferencia :: Eq a => Conj a -> Conj a -> Conj a
-- tal que (diferencia c1 c2) es el conjunto de los elementos de c1 que
-- no son elementos de c2. Por ejemplo,
--    diferencia ejConj1 ejConj2 == {0,3,5,7}
--    diferencia ejConj2 ejConj1 == {6,8}
-- ---------------------------------------------------------------------

diferencia :: Eq a => Conj a -> Conj a -> Conj a
diferencia (Cj xs) (Cj ys) = Cj zs
  where zs = [x | x <- xs, x `notElem` ys]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    diferenciaSimetrica :: Ord a => Conj a -> Conj a -> Conj a
-- tal que (diferenciaSimetrica c1 c2) es la diferencia simétrica de los
-- conjuntos c1 y c2. Por ejemplo,
--   diferenciaSimetrica ejConj1 ejConj2 == {0,3,5,6,7,8}
--   diferenciaSimetrica ejConj2 ejConj1 == {0,3,5,6,7,8}
-- ---------------------------------------------------------------------

diferenciaSimetrica :: Ord a => Conj a -> Conj a -> Conj a
diferenciaSimetrica c1 c2 =
  diferencia (union c1 c2) (interseccion c1 c2)

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    filtra :: (a -> Bool) -> Conj a -> Conj a
-- tal (filtra p c) es el conjunto de elementos de c que verifican el
-- predicado p. Por ejemplo,
--    filtra even ejConj1 == {0,2}
--    filtra odd  ejConj1  == {1,3,5,7,9}
-- ---------------------------------------------------------------------

filtra :: (a -> Bool) -> Conj a -> Conj a
filtra p (Cj xs) = Cj (filter p xs)

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    particion :: (a -> Bool) -> Conj a -> (Conj a, Conj a)
-- tal que (particion c) es el par formado por dos conjuntos: el de sus
-- elementos que verifican p y el de los elementos que no lo
-- verifica. Por ejemplo,
--    particion even ejConj1 == ({0,2},{1,3,5,7,9})
-- ---------------------------------------------------------------------

particion :: (a -> Bool) -> Conj a -> (Conj a, Conj a)
particion p c = (filtra p c, filtra (not . p) c)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    divide :: (Ord a) => a-> Conj a -> (Conj a, Conj a)
-- tal que (divide x c) es el par formado por dos subconjuntos de c: el
-- de los elementos menores o iguales que x y el de los mayores que x.
-- Por ejemplo,
--    divide 5 ejConj1 == ({0,1,2,3,5},{7,9})
-- ---------------------------------------------------------------------

divide :: Ord a => a-> Conj a -> (Conj a, Conj a)
divide x = particion (<= x)

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    mapC :: (a -> b) -> Conj a -> Conj b
-- tal que (map f c) es el conjunto formado por las imágenes de los
-- elementos de c, mediante f. Por ejemplo,
--   mapC (*2) (Cj [1..4]) == {2,4,6,8}
-- ---------------------------------------------------------------------

mapC :: (a -> b) -> Conj a -> Conj b
mapC f (Cj xs) = Cj (map f xs)

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    everyC :: (a -> Bool) -> Conj a -> Bool
-- tal que (everyC p c) se verifica si todos los elemsntos de c
-- verifican el predicado p.  Por ejmplo,
--   everyC even (Cj [2,4..10]) == True
--   everyC even (Cj [2..10])   == False
-- ---------------------------------------------------------------------

everyC :: (a -> Bool) -> Conj a -> Bool
everyC p (Cj xs) = all p xs

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    someC :: (a -> Bool) -> Conj a -> Bool
-- tal que (someC p c) se verifica si algún elemento de c verifica el
-- predicado p. Por ejemplo,
--   someC even (Cj [1,4,7]) == True
--   someC even (Cj [1,3,7]) == False
-- ---------------------------------------------------------------------

someC :: (a -> Bool) -> Conj a -> Bool
someC p (Cj xs) = any p xs

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    productoC :: (Ord a, Ord b) => Conj a -> Conj b -> Conj (a,b)
-- tal que (productoC c1 c2) es el producto cartesiano de los
-- conjuntos c1 y c2. Por ejemplo,
--   productoC (Cj [1,3]) (Cj [2,4])== {(1,2),(1,4),(3,2),(3,4)}
-- ---------------------------------------------------------------------

productoC :: (Ord a, Ord b) => Conj a -> Conj b -> Conj (a,b)
productoC (Cj xs) (Cj ys) =
  foldr inserta vacio [(x,y) | x <- xs, y <- ys]

-- ---------------------------------------------------------------------
-- Ejercicio. Especificar que, dado un tipo ordenado a, el orden entre
-- los conjuntos con elementos en a es el orden inducido por el orden
-- existente entre las listas con elementos en a.
-- ---------------------------------------------------------------------

instance Ord a => Ord (Conj a) where
  (Cj xs) <= (Cj ys) = xs <= ys

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    potencia :: Ord a => Conj a -> Conj (Conj a)
-- tal que (potencia c) es el conjunto potencia de c; es decir, el
-- conjunto de todos los subconjuntos de c. Por ejemplo,
--    potencia (Cj [1,2])  == {{},{1},{1,2},{2}}
--    potencia (Cj [1..3]) == {{},{1},{1,2},{1,2,3},{1,3},{2},{2,3},{3}}
-- ---------------------------------------------------------------------

potencia :: Ord a => Conj a -> Conj (Conj a)
potencia (Cj []) = unitario vacio
potencia (Cj (x:xs)) = mapC (inserta x) pr `union` pr
  where pr = potencia (Cj xs)

-- ---------------------------------------------------------------------
-- Ejercicio 20. Comprobar con QuickCheck que la relación de subconjunto
-- es un orden parcial. Es decir, es una relación reflexiva,
-- antisimétrica y transitiva.
-- ---------------------------------------------------------------------

propSubconjuntoReflexiva :: Conj Int -> Bool
propSubconjuntoReflexiva c = subconjunto c c

-- La comprobación es
--    λ> quickCheck propSubconjuntoReflexiva
--    +++ OK, passed 100 tests.

propSubconjuntoAntisimetrica :: Conj Int -> Conj Int -> Property
propSubconjuntoAntisimetrica c1 c2 =
  subconjunto c1 c2 && subconjunto c2 c1 ==> c1 == c2

-- La comprobación es
--    λ> quickCheck propSubconjuntoAntisimetrica
--    *** Gave up! Passed only 13 tests.

propSubconjuntoAntisimetrica2 :: Conj Int -> Conj Int -> Bool
propSubconjuntoAntisimetrica2 c1 c2 =
  not (subconjunto c1 c2 && subconjunto c2 c1) || (c1 == c2)

-- La comprobación es
--    λ> quickCheck propSubconjuntoAntisimetrica2
--    +++ OK, passed 100 tests.

propSubconjuntoTransitiva :: Conj Int -> Conj Int -> Conj Int -> Property
propSubconjuntoTransitiva c1 c2 c3 =
  subconjunto c1 c2 && subconjunto c2 c3 ==> subconjunto c1 c3

-- La comprobación es
--    λ> quickCheck propSubconjuntoTransitiva
--    *** Gave up! Passed only 7 tests.

propSubconjuntoTransitiva2 :: Conj Int -> Conj Int -> Conj Int -> Bool
propSubconjuntoTransitiva2 c1 c2 c3 =
  not (subconjunto c1 c2 && subconjunto c2 c3) || subconjunto c1 c3

-- La comprobación es
--    λ> quickCheck propSubconjuntoTransitiva2
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 21. Comprobar con QuickCheck que el conjunto vacío está
-- contenido en cualquier conjunto.
-- ---------------------------------------------------------------------

propSubconjuntoVacio :: Conj Int -> Bool
propSubconjuntoVacio c = subconjunto vacio c

-- La comprobación es
--    λ> quickCheck propSubconjuntoVacio
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 22. Comprobar con QuickCheck las siguientes propiedades de
-- la unión de conjuntos:
--    Idempotente:      A U A = A
--    Neutro:           A U {} = A
--    Commutativa:      A U B = B U A
--    Asociativa:       A U (B U C) = (A U B) U C
--    UnionSubconjunto: A y B son subconjuntos de (A U B)
--    UnionDiferencia:  A U B = A U (B \ A)
-- ---------------------------------------------------------------------

propUnionIdempotente :: Conj Int -> Bool
propUnionIdempotente c =
  union c c == c

-- La comprobación es
--    λ> quickCheck propUnionIdempotente
--    +++ OK, passed 100 tests.

propVacioNeutroUnion :: Conj Int -> Bool
propVacioNeutroUnion c =
  union c vacio == c

-- La comprobación es
--    λ> quickCheck propVacioNeutroUnion
--    +++ OK, passed 100 tests.

propUnionCommutativa :: Conj Int -> Conj Int -> Bool
propUnionCommutativa c1 c2 =
  union c1 c2 == union c2 c1

-- La comprobación es
--    λ> quickCheck propUnionCommutativa
--    +++ OK, passed 100 tests.

propUnionAsociativa :: Conj Int -> Conj Int -> Conj Int -> Bool
propUnionAsociativa c1 c2 c3 =
  union c1 (union c2 c3) == union (union c1 c2) c3

-- La comprobación es
--    λ> quickCheck propUnionAsociativa
--    +++ OK, passed 100 tests.

propUnionSubconjunto :: Conj Int -> Conj Int -> Bool
propUnionSubconjunto c1 c2 =
  subconjunto c1 c3 && subconjunto c2 c3
  where c3 = union c1 c2

-- La comprobación es
--    λ> quickCheck propUnionSubconjunto
--    +++ OK, passed 100 tests.

propUnionDiferencia :: Conj Int -> Conj Int -> Bool
propUnionDiferencia c1 c2 =
  union c1 c2 == union c1 (diferencia c2 c1)

-- La comprobación es
--    λ> quickCheck propUnionDiferencia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 23. Comprobar con QuickCheck las siguientes propiedades de
-- la intersección de conjuntos:
--    Idempotente:             A n A = A
--    VacioInterseccion:       A n {} = {}
--    Commutativa:             A n B = B n A
--    Asociativa:              A n (B n C) = (A n B) n C
--    InterseccionSubconjunto: (A n B) es subconjunto de A y B
--    DistributivaIU:          A n (B U C) = (A n B) U (A n C)
--    DistributivaUI:          A U (B n C) = (A U B) n (A U C)
-- ---------------------------------------------------------------------


propInterseccionIdempotente :: Conj Int -> Bool
propInterseccionIdempotente c =
  interseccion c c == c

-- La comprobación es
--    λ> quickCheck propInterseccionIdempotente
--    +++ OK, passed 100 tests.

propVacioInterseccion :: Conj Int -> Bool
propVacioInterseccion c =
  interseccion c vacio == vacio

-- La comprobación es
--    λ> quickCheck propVacioInterseccion
--    +++ OK, passed 100 tests.

propInterseccionCommutativa :: Conj Int -> Conj Int -> Bool
propInterseccionCommutativa c1 c2 =
  interseccion c1 c2 == interseccion c2 c1

-- La comprobación es
--    λ> quickCheck propInterseccionCommutativa
--    +++ OK, passed 100 tests.

propInterseccionAsociativa :: Conj Int -> Conj Int -> Conj Int -> Bool
propInterseccionAsociativa c1 c2 c3 =
  interseccion c1 (interseccion c2 c3) == interseccion (interseccion c1 c2) c3

-- La comprobación es
--    λ> quickCheck propInterseccionAsociativa
--    +++ OK, passed 100 tests.

propInterseccionSubconjunto :: Conj Int -> Conj Int -> Bool
propInterseccionSubconjunto c1 c2 =
  subconjunto c3 c1 && subconjunto c3 c2
  where c3 = interseccion c1 c2

-- La comprobación es
--    λ> quickCheck propInterseccionSubconjunto
--    +++ OK, passed 100 tests.

propDistributivaIU :: Conj Int -> Conj Int -> Conj Int -> Bool
propDistributivaIU c1 c2 c3 =
  interseccion c1 (union c2 c3) == union (interseccion c1 c2)
                                         (interseccion c1 c3)

-- La comprobación es
--    λ> quickCheck propDistributivaIU
--    +++ OK, passed 100 tests.

propDistributivaUI :: Conj Int -> Conj Int -> Conj Int -> Bool
propDistributivaUI c1 c2 c3 =
  union c1 (interseccion c2 c3) == interseccion (union c1 c2)
                                                (union c1 c3)
-- La comprobación es
--    λ> quickCheck propDistributivaUI
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 24. Comprobar con QuickCheck las siguientes propiedades de
-- la diferencia de conjuntos:
--    DiferenciaVacio1: A \ {} = A
--    DiferenciaVacio2: {} \ A = {}
--    DiferenciaDif1:   (A \ B) \ C = A \ (B U C)
--    DiferenciaDif2:   A \ (B \ C) = (A \ B) U (A n C)
--    DiferenciaSubc:   (A \ B) es subconjunto de A
--    DiferenciaDisj:   A y (B \ A) son disjuntos
--    DiferenciaUI:     (A U B) \ A = B \ (A n B)
-- ---------------------------------------------------------------------

propDiferenciaVacio1 :: Conj Int -> Bool
propDiferenciaVacio1 c = diferencia c vacio == c

-- La comprobación es
--    λ> quickCheck propDiferenciaVacio2
--    +++ OK, passed 100 tests.

propDiferenciaVacio2 :: Conj Int -> Bool
propDiferenciaVacio2 c = diferencia vacio c == vacio

-- La comprobación es
--    λ> quickCheck propDiferenciaVacio2
--    +++ OK, passed 100 tests.

propDiferenciaDif1 :: Conj Int -> Conj Int -> Conj Int -> Bool
propDiferenciaDif1 c1 c2 c3 =
  diferencia (diferencia c1 c2) c3 == diferencia c1 (union c2 c3)

-- La comprobación es
--    λ> quickCheck propDiferenciaDif1
--    +++ OK, passed 100 tests.

propDiferenciaDif2 :: Conj Int -> Conj Int -> Conj Int -> Bool
propDiferenciaDif2 c1 c2 c3 =
  diferencia c1 (diferencia c2 c3) == union (diferencia c1 c2)
                                            (interseccion c1 c3)

-- La comprobación es
--    λ> quickCheck propDiferenciaDif2
--    +++ OK, passed 100 tests.

propDiferenciaSubc :: Conj Int -> Conj Int -> Bool
propDiferenciaSubc c1 c2 =
  subconjunto (diferencia c1 c2) c1

-- La comprobación es
--    λ> quickCheck propDiferenciaSubc
--    +++ OK, passed 100 tests.

propDiferenciaDisj :: Conj Int -> Conj Int -> Bool
propDiferenciaDisj c1 c2 =
  disjuntos c1 (diferencia c2 c1)

-- La comprobación es
--    λ> quickCheck propDiferenciaDisj
--    +++ OK, passed 100 tests.

propDiferenciaUI :: Conj Int -> Conj Int -> Bool
propDiferenciaUI c1 c2 =
  diferencia (union c1 c2) c1 == diferencia c2 (interseccion c1 c2)

-- La comprobación es
--    λ> quickCheck propDiferenciaUI
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Generador de conjuntos                                          --
-- ---------------------------------------------------------------------

-- genConjunto es un generador de conjuntos. Por ejemplo,
--    λ> sample genConjunto
--    {}
--    {}
--    {}
--    {3,-2,-2,-3,-2,4}
--    {-8,0,4,6,-5,-2}
--    {12,-2,-1,-10,-2,2,15,15}
--    {2}
--    {}
--    {-42,55,55,-11,23,23,-11,27,-17,-48,16,-15,-7,5,41,43}
--    {-124,-66,-5,-47,58,-88,-32,-125}
--    {49,-38,-231,-117,-32,-3,45,227,-41,54,169,-160,19}
genConjunto :: Gen (Conj Int)
genConjunto = do
  xs <- listOf arbitrary
  return (foldr inserta vacio xs)

-- Los conjuntos son concreciones de los arbitrarios.
instance Arbitrary (Conj Int) where
  arbitrary = genConjunto
