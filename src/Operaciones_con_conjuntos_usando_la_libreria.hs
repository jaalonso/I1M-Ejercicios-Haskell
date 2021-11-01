-- Operaciones_con_conjuntos_usando_la_libreria.hs
-- Operaciones con conjuntos usando la librería Data.Set.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Operaciones_con_conjuntos_usando_la_libreria where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es hacer los ejercicios de la relación
-- anterior sobre operaciones con conjuntos usando la librería Data.Set

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Set as S

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    subconjunto :: Ord a => Set a -> Set a -> Bool
-- tal que (subconjunto c1 c2) se verifica si todos los elementos de c1
-- pertenecen a c2. Por ejemplo,
--    subconjunto (fromList [2..100000]) (fromList [1..100000]) == True
--    subconjunto (fromList [1..100000]) (fromList [2..100000]) == False
-- ---------------------------------------------------------------------

subconjunto :: Ord a => Set a -> Set a -> Bool
subconjunto = isSubsetOf

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    subconjuntoPropio :: Ord a => Conj a -> Conj a -> Bool
-- tal (subconjuntoPropio c1 c2) se verifica si c1 es un subconjunto
-- propio de c2. Por ejemplo,
--   subconjuntoPropio (fromList [2..5]) (fromList [1..7]) == True
--   subconjuntoPropio (fromList [2..5]) (fromList [1..4]) == False
--   subconjuntoPropio (fromList [2..5]) (fromList [2..5]) == False
-- ---------------------------------------------------------------------

subconjuntoPropio :: Ord a => Set a -> Set a -> Bool
subconjuntoPropio = isProperSubsetOf

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    unitario :: Ord a => a -> Set a
-- tal que (unitario x) es el conjunto {x}. Por ejemplo,
--   unitario 5 == fromList [5]
-- ---------------------------------------------------------------------

unitario :: Ord a => a -> Set a
unitario = singleton

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    cardinal :: Set a -> Int
-- tal que (cardinal c) es el número de elementos del conjunto c. Por
-- ejemplo,
--    cardinal (fromList [3,2,5,1,2,3])  ==  4
-- ---------------------------------------------------------------------

cardinal :: Set a -> Int
cardinal = size

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    union' :: Ord a => Set a -> Set a -> Set a
-- tal (union' c1 c2) es la unión de ambos conjuntos. Por ejemplo,
--    λ> union' (fromList [3,2,5]) (fromList [2,7,5])
--    fromList [2,3,5,7]
-- ---------------------------------------------------------------------

union' :: Ord a => Set a -> Set a -> Set a
union' = union

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    unionG:: Ord a => [Set a] -> Set a
-- tal (unionG cs) calcule la unión de la lista de conjuntos cd. Por
-- ejemplo,
--    λ> unionG [fromList [3,2], fromList [2,5], fromList [3,5,7]]
--    fromList [2,3,5,7]
-- ---------------------------------------------------------------------

unionG :: Ord a => [Set a] -> Set a
unionG = unions

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    interseccion :: Ord a => Set a -> Set a -> Set a
-- tal que (interseccion c1 c2) es la intersección de los conjuntos c1 y
-- c2. Por ejemplo,
--    λ> interseccion (fromList [1..7]) (fromList [4..9])
--    fromList [4,5,6,7]
--    λ> interseccion (fromList [2..1000000]) (fromList [1])
--    fromList []
-- ---------------------------------------------------------------------

interseccion :: Ord a => Set a -> Set a -> Set a
interseccion = intersection

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    interseccionG:: Ord a => [Set a] -> Set a
-- tal que (interseccionG cs) es la intersección de la lista de
-- conjuntos cs. Por ejemplo,
--    λ> interseccionG [fromList [3,2], fromList [2,5,3], fromList [3,5,7]]
--    fromList [3]
-- ---------------------------------------------------------------------

interseccionG :: Ord a => [Set a] -> Set a
interseccionG [c]      = c
interseccionG (cs:css) = intersection cs (interseccionG css)
interseccionG []       = error "Imposible"

-- Se puede definir por plegado
interseccionG2 :: Ord a => [Set a] -> Set a
interseccionG2 = foldr1 interseccion

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    disjuntos :: Ord a => Set a -> Set a -> Bool
-- tal que (disjuntos c1 c2) se verifica si los conjuntos c1 y c2 son
-- disjuntos. Por ejemplo,
--   disjuntos (fromList [2..5]) (fromList [6..9]) == True
--   disjuntos (fromList [2..5]) (fromList [1..9]) == False
-- ---------------------------------------------------------------------

disjuntos :: Ord a => Set a -> Set a -> Bool
disjuntos c1 c2 = S.null (intersection c1 c2)

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    diferencia :: Ord a => Set a -> Set a -> Set a
-- tal que (diferencia c1 c2) es el conjunto de los elementos de c1 que
-- no son elementos de c2. Por ejemplo,
--    λ> diferencia (fromList [2,5,3]) (fromList [1,4,5])
--    fromList [2,3]
-- ---------------------------------------------------------------------

diferencia :: Ord a => Set a -> Set a -> Set a
diferencia = difference

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    diferenciaSimetrica :: Ord a => Set a -> Set a -> Set a
-- tal que (diferenciaSimetrica c1 c2) es la diferencia simétrica de los
-- conjuntos c1 y c2. Por ejemplo,
--    λ> diferenciaSimetrica (fromList [3,2,5]) (fromList [1,5])
--    fromList [1,2,3]
-- ---------------------------------------------------------------------

diferenciaSimetrica :: Ord a => Set a -> Set a -> Set a
diferenciaSimetrica c1 c2 =
  (c1 `union` c2) \\ (c1 `intersection` c2)

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    filtra :: (a -> Bool) -> Set a -> Set a
-- tal (filtra p c) es el conjunto de elementos de c que verifican el
-- predicado p. Por ejemplo,
--    filtra even (fromList [3,2,5,6,8,9])  ==  fromList [2,6,8]
--    filtra odd  (fromList [3,2,5,6,8,9])  ==  fromList [3,5,9]
-- ---------------------------------------------------------------------

filtra :: (a -> Bool) -> Set a -> Set a
filtra = S.filter

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    particion :: (a -> Bool) -> Set a -> (Set a, Set a)
-- tal que (particion c) es el par formado por dos conjuntos: el de sus
-- elementos que verifican p y el de los elementos que no lo verifica.
-- Por ejemplo,
--    λ> particion even (fromList [3,2,5,6,8,9])
--    (fromList [2,6,8],fromList [3,5,9])
-- ---------------------------------------------------------------------

particion :: (a -> Bool) -> Set a -> (Set a, Set a)
particion = partition

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    divide :: (Ord a) => a-> Set a -> (Set a, Set a)
-- tal que (divide x c) es el par formado por dos subconjuntos de c: el
-- de los elementos menores que x y el de los mayores que x. Por ejemplo,
--    λ> divide 5 (fromList [3,2,9,5,8,6])
--    (fromList [2,3],fromList [6,8,9])
-- ---------------------------------------------------------------------

divide :: Ord a => a-> Set a -> (Set a, Set a)
divide = split

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir la función
--    mapC :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
-- tal que (map f c) es el conjunto formado por las imágenes de los
-- elementos de c, mediante f. Por ejemplo,
--    mapC (*2) (fromList [1..4])  ==  fromList [2,4,6,8]
-- ---------------------------------------------------------------------

mapC :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapC = S.map

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    everyC :: Ord a => (a -> Bool) -> Set a -> Bool
-- tal que (everyC p c) se verifica si todos los elementos de c
-- verifican el predicado p.  Por ejemplo,
--   everyC even (fromList [2,4..10]) == True
--   everyC even (fromList [2..10])   == False
-- ---------------------------------------------------------------------

-- 1ª definición
everyC :: Ord a => (a -> Bool) -> Set a -> Bool
everyC p c | S.null c  = True
           | otherwise = p x && everyC p c1
  where (x,c1) = deleteFindMin c

-- 2ª definición
everyC2 :: Ord a => (a -> Bool) -> Set a -> Bool
everyC2 p = S.foldr (\x r -> p x && r) True

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    someC :: Ord a => (a -> Bool) -> Set a -> Bool
-- tal que (someC p c) se verifica si algún elemento de c verifica el
-- predicado p. Por ejemplo,
--   someC even (fromList [1,4,7]) == True
--   someC even (fromList [1,3,7]) == False
-- ---------------------------------------------------------------------

-- 1ª definición
someC :: Ord a => (a -> Bool) -> Set a -> Bool
someC p c | S.null c  = False
          | otherwise = p x || someC p c1
  where (x,c1) = deleteFindMin c

-- 2ª definición
someC2 :: Ord a => (a -> Bool) -> Set a -> Bool
someC2 p = S.foldr (\x r -> p x || r) False

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    productoC :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
-- tal que (productoC c1 c2) es el producto cartesiano de los
-- conjuntos c1 y c2. Por ejemplo,
--    λ> productoC (fromList [1,3]) (fromList [2,4])
--    fromList [(1,2),(1,4),(3,2),(3,4)]
-- ---------------------------------------------------------------------

productoC :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
productoC c1 c2 =
  fromList [(x,y) | x <- elems c1, y <- elems c2]

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    potencia :: Ord a => Set a -> Set (Set a)
-- tal que (potencia c) es el conjunto potencia de c; es decir, el
-- conjunto de todos los subconjuntos de c. Por ejemplo,
--    λ> potencia (fromList [1..3])
--    fromList [fromList [],fromList [1],fromList [1,2],fromList [1,2,3],
--              fromList [1,3],fromList [2],fromList [2,3],fromList [3]]
-- ---------------------------------------------------------------------

potencia :: Ord a => Set a -> Set (Set a)
potencia c | S.null c  = singleton empty
           | otherwise = S.map (insert x) pr `union` pr
    where (x,rc) = deleteFindMin c
          pr     = potencia rc
