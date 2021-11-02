-- Relaciones_binarias_homogeneas.hs
-- Relaciones binarias homogéneas.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Relaciones_binarias_homogeneas where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir propiedades y
-- operaciones sobre las relaciones binarias (homogéneas).
--
-- Como referencia se puede usar el artículo de la wikipedia
-- http://bit.ly/HVHOPS

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck (quickCheck, (==>), Property)
import Data.List (union)

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una relación binaria R sobre un conjunto A puede
-- representar mediante un par (xs,ps) donde xs es la lista de los
-- elementos de A (el universo de R) y ps es la lista de pares de R (el
-- grafo de R). Definir el tipo de dato (Rel a) para representar las
-- relaciones binarias sobre a.
-- ---------------------------------------------------------------------

type Rel a = ([a],[(a,a)])

-- ---------------------------------------------------------------------
-- Nota. En los ejemplos usaremos las siguientes relaciones binarias:
--    r1, r2, r3 :: Rel Int
--    r1 = ([1..9],[(1,3), (2,6), (8,9), (2,7)])
--    r2 = ([1..9],[(1,3), (2,6), (8,9), (3,7)])
--    r3 = ([1..9],[(1,3), (2,6), (8,9), (3,6)])
-- ---------------------------------------------------------------------

r1, r2, r3 :: Rel Int
r1 = ([1..9],[(1,3), (2,6), (8,9), (2,7)])
r2 = ([1..9],[(1,3), (2,6), (8,9), (3,7)])
r3 = ([1..9],[(1,3), (2,6), (8,9), (3,6)])

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    universo :: Eq a => Rel a -> [a]
-- tal que (universo r) es el universo de la relación r. Por ejemplo,
--    r1           ==  ([1,2,3,4,5,6,7,8,9],[(1,3),(2,6),(8,9),(2,7)])
--    universo r1  ==  [1,2,3,4,5,6,7,8,9]
-- ---------------------------------------------------------------------

universo :: Eq a => Rel a -> [a]
universo (us,_) = us

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    grafo :: Eq a => ([a],[(a,a)]) -> [(a,a)]
-- tal que (grafo r) es el grafo de la relación r. Por ejemplo,
--    r1        ==  ([1,2,3,4,5,6,7,8,9],[(1,3),(2,6),(8,9),(2,7)])
--    grafo r1  ==  [(1,3),(2,6),(8,9),(2,7)]
-- ---------------------------------------------------------------------

grafo :: Eq a => Rel a -> [(a,a)]
grafo (_,ps) = ps

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    reflexiva :: Eq a => Rel a -> Bool
-- tal que (reflexiva r) se verifica si la relación r es reflexiva. Por
-- ejemplo,
--    reflexiva ([1,3],[(1,1),(1,3),(3,3)])    ==  True
--    reflexiva ([1,2,3],[(1,1),(1,3),(3,3)])  ==  False
-- ---------------------------------------------------------------------

reflexiva :: Eq a => Rel a -> Bool
reflexiva (us,ps) = and [(x,x) `elem` ps | x <- us]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    simetrica :: Eq a => Rel a -> Bool
-- tal que (simetrica r) se verifica si la relación r es simétrica. Por
-- ejemplo,
--    simetrica ([1,3],[(1,1),(1,3),(3,1)])  ==  True
--    simetrica ([1,3],[(1,1),(1,3),(3,2)])  ==  False
--    simetrica ([1,3],[])                   ==  True
-- ---------------------------------------------------------------------

simetrica :: Eq a => Rel a -> Bool
simetrica (_,ps) = and [(y,x) `elem` ps | (x,y) <- ps]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    subconjunto :: Eq a => [a] -> [a] -> Bool
-- tal que (subconjunto xs ys) se verifica si xs es un subconjunto de
-- xs. Por ejemplo,
--    subconjunto [1,3] [3,1,5]  ==  True
--    subconjunto [3,1,5] [1,3]  ==  False
-- ---------------------------------------------------------------------

subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [x `elem` ys | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    composicion :: Eq a => Rel a -> Rel a -> Rel a
-- tal que (composicion r s) es la composición de las relaciones r y
-- s. Por ejemplo,
--    λ> composicion ([1,2],[(1,2),(2,2)]) ([1,2],[(2,1)])
--    ([1,2],[(1,1),(2,1)])
-- ---------------------------------------------------------------------

composicion :: Eq a => Rel a -> Rel a -> Rel a
composicion (xs,ps) (_,qs) =
  (xs,[(x,z) | (x,y) <- ps, (y',z) <- qs, y == y'])

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    transitiva :: Eq a => Rel a -> Bool
-- tal que (transitiva r) se verifica si la relación r es transitiva.
-- Por ejemplo,
--    transitiva ([1,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)])  ==  True
--    transitiva ([1,3,5],[(1,1),(1,3),(3,1),(5,5)])        ==  False
-- ---------------------------------------------------------------------

transitiva :: Eq a => Rel a -> Bool
transitiva r@(_,ps) = subconjunto (grafo (composicion r r)) ps

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    esEquivalencia :: Eq a => Rel a -> Bool
-- tal que (esEquivalencia r) se verifica si la relación r es de
-- equivalencia. Por ejemplo,
--    λ> esEquivalencia ([1,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)])
--    True
--    λ> esEquivalencia ([1,2,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)])
--    False
--    λ> esEquivalencia ([1,3,5],[(1,1),(1,3),(3,3),(5,5)])
--    False
-- ---------------------------------------------------------------------

esEquivalencia :: Eq a => Rel a -> Bool
esEquivalencia r = reflexiva r && simetrica r && transitiva r

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    irreflexiva :: Eq a => Rel a -> Bool
-- tal que (irreflexiva r) se verifica si la relación r es irreflexiva;
-- es decir, si ningún elemento de su universo está relacionado con
-- él mismo. Por ejemplo,
--    irreflexiva ([1,2,3],[(1,2),(2,1),(2,3)])  ==  True
--    irreflexiva ([1,2,3],[(1,2),(2,1),(3,3)])  ==  False
-- ---------------------------------------------------------------------

irreflexiva :: Eq a => Rel a -> Bool
irreflexiva (xs,ps) = and [(x,x) `notElem` ps | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    antisimetrica :: Eq a => Rel a -> Bool
-- tal que (antisimetrica r) se verifica si la relación r es
-- antisimétrica; es decir, si (x,y) e (y,x) están relacionado, entonces
-- x=y. Por ejemplo,
--    antisimetrica ([1,2],[(1,2)])        ==  True
--    antisimetrica ([1,2],[(1,2),(2,1)])  ==  False
--    antisimetrica ([1,2],[(1,1),(2,1)])  ==  True
-- ---------------------------------------------------------------------

antisimetrica :: Eq a => Rel a -> Bool
antisimetrica (_,ps) =
  null [(x,y) | (x,y) <- ps, x /= y, (y,x) `elem` ps]

-- 2ª definición
antisimetrica2 :: Eq a => Rel a -> Bool
antisimetrica2 (_,ps) = and [(y,x) `notElem` ps | (x,y) <- ps, x /= y]

-- 3ª definición
antisimetrica3 :: Eq a => Rel a -> Bool
antisimetrica3 (xs,ps) =
  and [((x,y) `elem` ps && (y,x) `elem` ps) --> (x == y)
       | x <- xs, y <- xs]
  where p --> q = not p || q

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    total :: Eq a => Rel a -> Bool
-- tal que (total r) se verifica si la relación r es total; es decir, si
-- para cualquier par x, y de elementos del universo de r, se tiene que
-- x está relacionado con y ó y etá relacionado con x. Por ejemplo,
--    total ([1,3],[(1,1),(3,1),(3,3)])  ==  True
--    total ([1,3],[(1,1),(3,1)])        ==  False
--    total ([1,3],[(1,1),(3,3)])        ==  False
-- ---------------------------------------------------------------------

total :: Eq a => Rel a -> Bool
total (xs,ps) =
  and [(x,y) `elem` ps || (y,x) `elem` ps | x <- xs, y <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 13. Comprobar con QuickCheck que las relaciones totales son
-- reflexivas.
-- ---------------------------------------------------------------------

prop_total_reflexiva :: Rel Int -> Property
prop_total_reflexiva r =
  total r ==> reflexiva r

-- La comprobación es
--    λ> quickCheck prop_total_reflexiva
--    *** Gave up! Passed only 19 tests.

-- ---------------------------------------------------------------------
-- § Clausuras                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    clausuraReflexiva :: Eq a => Rel a -> Rel a
-- tal que (clausuraReflexiva r) es la clausura reflexiva de r; es
-- decir, la menor relación reflexiva que contiene a r. Por ejemplo,
--    λ> clausuraReflexiva ([1,3],[(1,1),(3,1)])
--    ([1,3],[(1,1),(3,1),(3,3)])
-- ---------------------------------------------------------------------

clausuraReflexiva :: Eq a => Rel a -> Rel a
clausuraReflexiva (xs,ps) =
  (xs, ps `union` [(x,x) | x <- xs])

-- ---------------------------------------------------------------------
-- Ejercicio 15. Comprobar con QuickCheck que clausuraReflexiva es
-- reflexiva.
-- ---------------------------------------------------------------------

prop_ClausuraReflexiva :: Rel Int -> Bool
prop_ClausuraReflexiva r =
  reflexiva (clausuraReflexiva r)

-- La comprobación es
--    λ> quickCheck prop_ClausuraReflexiva
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 16. Definir la función
--    clausuraSimetrica :: Eq a => Rel a -> Rel a
-- tal que (clausuraSimetrica r) es la clausura simétrica de r; es
-- decir, la menor relación simétrica que contiene a r. Por ejemplo,
--    λ> clausuraSimetrica ([1,3,5],[(1,1),(3,1),(1,5)])
--    ([1,3,5],[(1,1),(3,1),(1,5),(1,3),(5,1)])
-- ---------------------------------------------------------------------

clausuraSimetrica :: Eq a => Rel a -> Rel a
clausuraSimetrica (xs,ps) =
  (xs, ps `union` [(y,x) | (x,y) <- ps])

-- ---------------------------------------------------------------------
-- Ejercicio 17. Comprobar con QuickCheck que clausuraSimetrica es
-- simétrica.
-- ---------------------------------------------------------------------

prop_ClausuraSimetrica :: Rel Int -> Bool
prop_ClausuraSimetrica r =
  simetrica (clausuraSimetrica r)

-- La comprobación es
--    λ> quickCheck prop_ClausuraSimetrica
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    clausuraTransitiva :: Eq a => Rel a -> Rel a
-- tal que (clausuraTransitiva r) es la clausura transitiva de r; es
-- decir, la menor relación transitiva que contiene a r. Por ejemplo,
--    λ> clausuraTransitiva ([1..6],[(1,2),(2,5),(5,6)])
--    ([1,2,3,4,5,6],[(1,2),(2,5),(5,6),(1,5),(2,6),(1,6)])
-- ---------------------------------------------------------------------

clausuraTransitiva :: Eq a => Rel a -> Rel a
clausuraTransitiva (xs,ps) = (xs, aux ps)
  where aux xs' | cerradoTr xs' = xs'
                | otherwise    = aux (xs' `union` comp xs' xs')
        cerradoTr r = subconjunto (comp r r) r
        comp r s    = [(x,z) | (x,y) <- r, (y',z) <- s, y == y']

-- ---------------------------------------------------------------------
-- Ejercicio 19. Comprobar con QuickCheck que clausuraTransitiva es
-- transitiva.
-- ---------------------------------------------------------------------

prop_ClausuraTransitiva :: Rel Int -> Bool
prop_ClausuraTransitiva r =
  transitiva (clausuraTransitiva r)

-- La comprobación es
--    λ> quickCheck prop_ClausuraTransitiva
--    +++ OK, passed 100 tests.
