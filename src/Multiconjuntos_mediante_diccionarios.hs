-- Multiconjuntos_mediante_diccionarios.hs
-- El tipo abstracto de los multiconjuntos mediante diccionarios.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Multiconjuntos_mediante_diccionarios where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Un multiconjunto es una colección de elementos en los que no importa
-- el orden de los elementos, pero sí el número de veces en que
-- aparecen. Por ejemplo, la factorización prima de un número se puede
-- representar como un multiconjunto de números primos.
--
-- El objetivo de esta relación de ejercicios es implementar el TAD de
-- los multiconjuntos utilizando los diccionarios estudiados en el tema
-- 29 https://jaalonso.github.io/cursos/i1m/temas/tema-29.html
--
-- El manual, con ejemplos, de la librería Data.Map se encuentra en
-- http://bit.ly/25B1na0

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- El tipo de dato de multiconjuntos                                  --
-- ---------------------------------------------------------------------

-- Un multiconjunto se puede representar mediante un diccionario donde
-- las claves son los elementos del multiconjunto y sus valores sus
-- números de ocurrencias. Por ejemplo, el multiconjunto
--    {a, b, a, c, b, a, e}
-- se representa por el diccionario
--    [(a,3), (b,2), (c,1), (e,1)]

type MultiConj a = M.Map a Int

-- ---------------------------------------------------------------------
-- Construcciones de multiconjuntos                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la constante
--    vacio :: MultiConj a
-- para el multiconjunto vacío. Por ejemplo,
--    vacio  ==  fromList []
-- ---------------------------------------------------------------------

vacio :: MultiConj a
vacio = M.empty

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    unitario :: a -> MultiConj a
-- tal que (unitario x) es el multiconjunto cuyo único elemento es
-- x. Por ejemplo,
--    unitario 'a'  ==  fromList [('a',1)]
-- ---------------------------------------------------------------------

unitario :: a -> MultiConj a
unitario x = M.singleton x 1

-- ---------------------------------------------------------------------
-- Añadir y quitar elementos                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    inserta :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (inserta x m) es el multiconjunto obtenido añadiéndole a m el
-- elemento x. Por ejemplo,
--    λ> inserta 'a' (unitario 'a')
--    fromList [('a',2)]
--    λ> inserta 'b' it
--    fromList [('a',2),('b',1)]
--    λ> inserta 'a' it
--    fromList [('a',3),('b',1)]
--    λ> inserta 'b' it
--    fromList [('a',3),('b',2)]
-- ---------------------------------------------------------------------

inserta :: Ord a => a -> MultiConj a -> MultiConj a
inserta x = M.insertWith (+) x 1

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    listaAmc :: Ord a => [a] -> MultiConj a
-- tal que (listaAmc xs) es el multiconjunto cuyos elementos son los de
-- la lista xs. Por ejemplo,
--    listaAmc "ababc"  ==  fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

-- 1ª solución
listaAmc :: Ord a => [a] -> MultiConj a
listaAmc xs = M.fromListWith (+) (zip xs (repeat 1))

-- 2ª solución
listaAmc2 :: Ord a => [a] -> MultiConj a
listaAmc2 = foldr inserta vacio

-- Comparación de eficiencia
--    λ> listaAmc (replicate 5000000 1)
--    fromList [(1,5000000)]
--    (1.52 secs, 1,368,870,760 bytes)
--    λ> listaAmc2 (replicate 5000000 1)
--    fromList [(1,5000000)]
--    (4.20 secs, 2,385,729,056 bytes)
--
--    λ> listaAmc (replicate 10000000 1)
--    fromList [(1,10000000)]
--    (2.97 secs, 2,732,899,360 bytes)
--    λ> listaAmc2 (replicate 10000000 1)
--    fromList *** Exception: stack overflow

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
-- tal que (insertaVarios x n m) es el multiconjunto obtenido
-- añadiéndole a m n copias del elemento x. Por ejemplo,
--    λ> insertaVarios 'a' 3 vacio
--    fromList [('a',3)]
--    λ> insertaVarios 'b' 2 it
--    fromList [('a',3),('b',2)]
--    λ> insertaVarios 'a' 2 it
--    fromList [('a',5),('b',2)]
-- ---------------------------------------------------------------------

-- 1ª solución
insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
insertaVarios = M.insertWith (+)

-- 2ª solución
insertaVarios2 :: Ord a => a -> Int -> MultiConj a -> MultiConj a
insertaVarios2 x n m = foldr inserta m (replicate n x)

-- Comparación de eficiencia
--    λ> insertaVarios 1 5000000 vacio
--    fromList [(1,5000000)]
--    (0.00 secs, 0 bytes)
--    λ> insertaVarios2 1 5000000 vacio
--    fromList [(1,5000000)]
--    (4.24 secs, 2,226,242,792 bytes)
--
--    λ> insertaVarios 1 10000000 vacio
--    fromList [(1,10000000)]
--    (0.00 secs, 0 bytes)
--    λ> insertaVarios2 1 10000000 vacio
--    fromList *** Exception: stack overflow

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    borra :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borra x m) es el multiconjunto obtenido borrando una
-- ocurrencia de x en m. Por ejemplo,
--    λ> borra 'a' (listaAmc "ababc")
--    fromList [('a',1),('b',2),('c',1)]
--    λ> borra 'a' it
--    fromList [('b',2),('c',1)]
--    λ> borra 'a' it
--    fromList [('b',2),('c',1)]
-- ---------------------------------------------------------------------

borra :: Ord a => a -> MultiConj a -> MultiConj a
borra = M.update f
  where f m | m <= 1    = Nothing
            | otherwise = Just (m - 1)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
-- tal que (borraVarias x n m) es el multiconjunto obtenido a partir del
-- m borrando n ocurrencias del elemento x. Por ejemplo,
--    λ> listaAmc "ababcad"
--    fromList [('a',3),('b',2),('c',1),('d',1)]
--    λ> borraVarias 'a' 2 (listaAmc "ababcad")
--    fromList [('a',1),('b',2),('c',1),('d',1)]
--    λ> borraVarias 'a' 5 (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

-- 1ª definición
borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias x n = M.update (f n) x
  where f n' m | m <= n'   = Nothing
               | otherwise = Just (m - n')

-- 2ª definición
borraVarias2 :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias2 x n m = foldr borra m (replicate n x)

-- Comparación de eficiencia
--    λ> borraVarias 1 5000000 (listaAmc (replicate 6000000 1))
--    fromList [(1,1000000)]
--    (1.74 secs, 1,594,100,344 bytes)
--    λ> borraVarias2 1 5000000 (listaAmc (replicate 6000000 1))
--    fromList [(1,1000000)]
--    (6.79 secs, 4,424,846,104 bytes)
--
--    λ> borraVarias 1 5000000 (listaAmc (replicate 10000000 1))
--    fromList [(1,5000000)]
--    (3.02 secs, 2,768,894,680 bytes)
--    λ> borraVarias2 1 5000000 (listaAmc (replicate 10000000 1))
--    fromList *** Exception: stack overflow

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borraTodas x m) es el multiconjunto obtenido a partir del
-- m borrando todas las ocurrencias del elemento x. Por ejemplo,
--    λ> borraTodas 'a' (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
borraTodas = M.delete

-- ---------------------------------------------------------------------
-- Consultas                                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    esVacio :: MultiConj a -> Bool
-- tal que (esVacio m) se verifica si el multiconjunto m es vacío. Por
-- ejemplo,
--    esVacio vacio  ==  True
--    esVacio (inserta 'a' vacio)  ==  False
-- ---------------------------------------------------------------------

esVacio :: MultiConj a -> Bool
esVacio = M.null

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    cardinal :: MultiConj a -> Int
-- tal que (cardinal m) es el número de elementos (contando las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardinal (listaAmc "ababcad")  ==  7
-- ---------------------------------------------------------------------

cardinal :: MultiConj a -> Int
cardinal = sum . M.elems

-- 2ª definición
cardinal2 :: MultiConj a -> Int
cardinal2 m = sum [v | (_,v) <- M.assocs m]

-- Comparación de eficiencia
--    λ> cardinal (listaAmc [1..5000000])
--    5000000
--    (5.92 secs, 9,071,879,144 bytes)
--    λ> cardinal2 (listaAmc [1..5000000])
--    5000000
--    (7.06 secs, 9,591,013,280 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    cardDistintos :: MultiConj a -> Int
-- tal que (cardDistintos m) es el número de elementos (sin contar las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardDistintos (listaAmc "ababcad")  ==  4
-- ---------------------------------------------------------------------

-- 1ª definición
cardDistintos :: MultiConj a -> Int
cardDistintos = M.size

-- 2ª definición
cardDistintos2 :: MultiConj a -> Int
cardDistintos2 = length . M.keys

-- Comparación de eficiencia
--    λ> cardDistintos (listaAmc [1..10000000])
--    10000000
--    (9.86 secs, 17,538,021,680 bytes)
--    λ> cardDistintos2 (listaAmc [1..10000000])
--    10000000
--    (10.14 secs, 18,092,597,184 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    pertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (pertenece x m) se verifica si el elemento x pertenece al
-- multiconjunto m. Por ejemplo,
--    pertenece 'b' (listaAmc "ababcad")  ==  True
--    pertenece 'r' (listaAmc "ababcad")  ==  False
-- ---------------------------------------------------------------------

pertenece :: Ord a => a -> MultiConj a -> Bool
pertenece = M.member

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    noPertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (noPertenece x m) se verifica si el elemento x no pertenece al
-- multiconjunto m. Por ejemplo,
--    noPertenece 'b' (listaAmc "ababcad")  ==  False
--    noPertenece 'r' (listaAmc "ababcad")  ==  True
-- ---------------------------------------------------------------------

noPertenece :: Ord a => a -> MultiConj a -> Bool
noPertenece  = M.notMember

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    ocurrencias :: Ord a => a -> MultiConj a -> Int
-- tal que (ocurrencias x m) es el número de ocurrencias de x en el
-- multiconjunto m. Por ejemplo,
--    ocurrencias 'a' (listaAmc "ababcad")  ==  3
--    ocurrencias 'r' (listaAmc "ababcad")  ==  0
-- ---------------------------------------------------------------------

ocurrencias :: Ord a => a -> MultiConj a -> Int
ocurrencias = M.findWithDefault 0

-- ---------------------------------------------------------------------
-- Ejercicio 15: Definir la función
--    elementos :: Ord a => MultiConj a -> [a]
-- tal que (elementos m) es la lista de los elementos (sin repeticiones)
-- del multiconjunto m. Por ejemplo,
--    elementos (listaAmc "ababcad")  ==  "abcd"
-- ---------------------------------------------------------------------

elementos :: Ord a => MultiConj a -> [a]
elementos = M.keys

-- ---------------------------------------------------------------------
-- Ejercicio 16.Definir la función
--    esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
-- tal que (esSubmultiConj m1 m2) se verifica si m1 es un
-- submulticonjuto de m2 (es decir; los elementos de m1 pertenecen a m2
-- con un númro de ocurrencias igual o mayor). Por ejemplo,
--    λ> let m1 = listaAmc "ababcad"
--    λ> let m2 = listaAmc "bcbaadaa"
--    λ> m1
--    fromList [('a',3),('b',2),('c',1),('d',1)]
--    λ> m2
--    fromList [('a',4),('b',2),('c',1),('d',1)]
--    λ> esSubmultiConj m1 m2
--    True
--    λ> esSubmultiConj m2 m1
--    False
-- ---------------------------------------------------------------------

-- 1ª definición
esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
esSubmultiConj m1 m2 =
  all (\x -> ocurrencias x m1 <= ocurrencias x m2)
      (elementos m1)

-- 2ª definición
esSubmultiConj2 :: Ord a => MultiConj a -> MultiConj a -> Bool
esSubmultiConj2 = M.isSubmapOfBy (<=)

-- Comparación de eficiencia
--    λ> esSubmultiConj (listaAmc [1..1000000]) (listaAmc [1..1000000])
--    True
--    (3.06 secs, 3,440,710,816 bytes)
--    λ> esSubmultiConj2 (listaAmc [1..1000000]) (listaAmc [1..1000000])
--    True
--    (1.71 secs, 3,058,187,728 bytes)
--
--    λ> let m = listaAmc (replicate 10000000 1) in esSubmultiConj m m
--    True
--    (5.71 secs, 5,539,250,712 bytes)
--    λ> let m = listaAmc (replicate 10000000 1) in esSubmultiConj2 m m
--    True
--    (5.87 secs, 5,468,766,496 bytes)

-- ---------------------------------------------------------------------
-- Elemento minimo y máximo de un multiconjunto                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    minimo :: MultiConj a -> a
-- tal que (minimo m) es el mínimo elemento del multiconjunto m. Por
-- ejemplo,
--    minimo (listaAmc "cdacbab")  ==  'a'
-- ---------------------------------------------------------------------

minimo :: MultiConj a -> a
minimo = fst . M.findMin

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    maximo :: MultiConj a -> a
-- tal que (maximo m) es el máximo elemento del multiconjunto m. Por
-- ejemplo,
--    maximo (listaAmc "cdacbab")  ==  'd'
-- ---------------------------------------------------------------------

maximo :: MultiConj a -> a
maximo = fst . M.findMax

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    borraMin :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMin m) es el multiconjunto obtenido eliminando una
-- ocurrencia del menor elemento de m. Por ejemplo,
--    λ> borraMin (listaAmc "cdacbab")
--    fromList [('a',1),('b',2),('c',2),('d',1)]
--    λ> borraMin it
--    fromList [('b',2),('c',2),('d',1)]
-- ---------------------------------------------------------------------

borraMin :: Ord a => MultiConj a -> MultiConj a
borraMin m = borra (minimo m) m

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    borraMax :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMax m) es el multiconjunto obtenido eliminando una
-- ocurrencia del mayor elemento de m. Por ejemplo,
--    λ> borraMax (listaAmc "cdacbab")
--    fromList [('a',2),('b',2),('c',2)]
--    λ> borraMax it
--    fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

borraMax :: Ord a => MultiConj a -> MultiConj a
borraMax m = borra (maximo m) m

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--    borraMinTodo :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMinTodo m) es el multiconjunto obtenido eliminando
-- todas las ocurrencias del menor elemento de m. Por ejemplo,
--    λ> borraMinTodo (listaAmc "cdacbab")
--    fromList [('b',2),('c',2),('d',1)]
--    λ> borraMinTodo it
--    fromList [('c',2),('d',1)]
-- ---------------------------------------------------------------------

borraMinTodo :: Ord a => MultiConj a -> MultiConj a
borraMinTodo = M.deleteMin

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--    borraMaxTodo :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMaxTodo m) es el multiconjunto obtenido eliminando
-- todas las ocurrencias del mayor elemento de m. Por ejemplo,
--    λ> borraMaxTodo (listaAmc "cdacbab")
--    fromList [('a',2),('b',2),('c',2)]
--    λ> borraMaxTodo it
--    fromList [('a',2),('b',2)]
-- ---------------------------------------------------------------------

borraMaxTodo :: Ord a => MultiConj a -> MultiConj a
borraMaxTodo = M.deleteMax

-- ---------------------------------------------------------------------
-- Operaciones: unión, intersección y diferencia de multiconjuntos    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (union m1 m2) es la unión de los multiconjuntos m1 y m2. Por
-- ejemplo,
--    λ> let m1 = listaAmc "cdacba"
--    λ> let m2 = listaAmc "acec"
--    λ> m1
--    fromList [('a',2),('b',1),('c',2),('d',1)]
--    λ> m2
--    fromList [('a',1),('c',2),('e',1)]
--    λ> union m1 m2
--    fromList [('a',3),('b',1),('c',4),('d',1),('e',1)]
-- ---------------------------------------------------------------------

union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
union = M.unionWith (+)

-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--    unionG :: Ord a => [MultiConj a] -> MultiConj a
-- tal que (unionG ms) es la unión de la lista de multiconjuntos ms. Por
-- ejemplo,
--    λ> unionG (map listaAmc ["aba", "cda", "bdb"])
--    fromList [('a',3),('b',3),('c',1),('d',2)]
-- ---------------------------------------------------------------------

-- 1ª definición
unionG :: Ord a => [MultiConj a] -> MultiConj a
unionG = M.unionsWith (+)

-- 2ª definición
unionG2 :: Ord a => [MultiConj a] -> MultiConj a
unionG2 = foldr union vacio

-- Comparación de eficiencia
--    λ> unionG (replicate 1000000 (listaAmc "abc"))
--    fromList [('a',1000000),('b',1000000),('c',1000000)]
--    (1.04 secs, 693,213,488 bytes)
--    λ> unionG2 (replicate 1000000 (listaAmc "abc"))
--    fromList [('a',1000000),('b',1000000),('c',1000000)]
--    (1.40 secs, 832,739,480 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (diferencia m1 m2) es la diferencia de los multiconjuntos m1
-- y m2. Por ejemplo,
--    λ> diferencia (listaAmc "abacc") (listaAmc "dcb")
--    fromList [('a',2),('c',1)]
-- ---------------------------------------------------------------------

diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
diferencia = M.differenceWith f
  where f x y | x <= y    = Nothing
              | otherwise = Just (x - y)

-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--    interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (interseccion m1 m2) es la intersección de los multiconjuntos
-- m1 y m2. Por ejemplo,
--    λ> interseccion (listaAmc "abcacc") (listaAmc "bdcbc")
--    fromList [('b',1),('c',2)]
-- ---------------------------------------------------------------------

interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
interseccion = M.intersectionWith min

-- ---------------------------------------------------------------------
-- Filtrado y partición                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--    filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
-- tal que (filtra p m) es el multiconjunto de los elementos de m que
-- verifican la propiedad p. Por ejemplo,
--    λ> filtra (>'b') (listaAmc "abaccaded")
--    fromList [('c',2),('d',2),('e',1)]
-- ---------------------------------------------------------------------

filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
filtra p = M.filterWithKey (\k _ -> p k)

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    particion :: Ord a =>
--                 (a -> Bool) -> MultiConj a -> (MultiConj a,MultiConj a)
-- tal que (particion p m) es el par cuya primera componente consta de
-- los elementos de m que cumplen p y la segunda por los que no lo
-- cumplen. Por ejemplo,
--    λ> particion (>'b') (listaAmc "abaccaded")
--    (fromList [('c',2),('d',2),('e',1)],fromList [('a',3),('b',1)])
-- ---------------------------------------------------------------------

particion :: Ord a =>
             (a -> Bool) -> MultiConj a -> (MultiConj a,MultiConj a)
particion p = M.partitionWithKey (\k _ -> p k)

-- ---------------------------------------------------------------------
-- Función aplicativa                                                 --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 29. Definir la función
--    mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
-- tal que (mapMC f m) es el multiconjunto obtenido aplicando la función
-- f a todos los  elementos de m. Por ejemplo,
--    λ> mapMC (:"N") (listaAmc "abaccaded")
--    fromList [("aN",3),("bN",1),("cN",2),("dN",2),("eN",1)]
-- ---------------------------------------------------------------------

mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
mapMC = M.mapKeys
