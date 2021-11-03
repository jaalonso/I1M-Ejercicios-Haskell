-- Relaciones_binarias_homogeneas_con_la_libreria.hs
-- Relaciones binarias homogéneas con la librería Data.Set.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

module Relaciones_binarias_homogeneas_con_la_libreria where

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es hacer los ejercicios de la relación
-- anterior sobre las relaciones binarias (homogéneas) usando la
-- librería Data.Set
--
-- Como referencia se puede usar el artículo de la wikipedia
-- http://bit.ly/HVHOPS

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.Set as S

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una relación binaria S sobre un conjunto A se puede
-- representar mediante la expresión (R xs ps) donde xs es el conjunto
-- de los elementos de A (el universo de S) y ps es el conjunto de pares
-- de S (el grafo de S). Definir el tipo de dato (Rel a) para
-- representar las relaciones binarias sobre a.
-- ---------------------------------------------------------------------

data Rel a = R (Set a) (Set (a,a))
  deriving Show

-- ---------------------------------------------------------------------
-- Nota. En los ejemplos usaremos las siguientes relaciones binarias:
--    r1, r2, r3 :: Rel Int
--    r1 = R (fromList [1..9]) (fromList [(1,3), (2,6), (8,9), (2,7)])
--    r2 = R (fromList [1..9]) (fromList [(1,3), (2,6), (8,9), (3,7)])
--    r3 = R (fromList [1..9]) (fromList [(1,3), (2,6), (8,9), (3,6)])
-- ---------------------------------------------------------------------

r1, r2, r3 :: Rel Int
r1 = R (fromList [1..9]) (fromList [(1,3), (2,6), (8,9), (2,7)])
r2 = R (fromList [1..9]) (fromList [(1,3), (2,6), (8,9), (3,7)])
r3 = R (fromList [1..9]) (fromList [(1,3), (2,6), (8,9), (3,6)])

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    universo :: Ord a => Rel a -> Set a
-- tal que (universo r) es el universo de la relación r. Por ejemplo,
--    universo r1  ==  fromList [1,2,3,4,5,6,7,8,9]
-- ---------------------------------------------------------------------

universo :: Ord a => Rel a -> Set a
universo (R u _) = u

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    grafo :: Ord a => Rel a -> [(a,a)]
-- tal que (grafo r) es el grafo de la relación r. Por ejemplo,
--    grafo r1  ==  fromList [(1,3),(2,6),(2,7),(8,9)]
-- ---------------------------------------------------------------------

grafo :: Ord a => Rel a -> Set (a,a)
grafo (R _ g) = g

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    reflexiva :: Ord a => Rel a -> Bool
-- tal que (reflexiva r) se verifica si la relación r es reflexiva. Por
-- ejemplo,
--    λ> reflexiva (R (fromList [1,3]) (fromList [(1,1),(1,3),(3,3)]))
--    True
--    λ> reflexiva (R (fromList [1,2,3]) (fromList [(1,1),(1,3),(3,3)]))
--    False
-- ---------------------------------------------------------------------

reflexiva :: Ord a => Rel a -> Bool
reflexiva (R u g) = and [(x,x) `member` g | x <- elems u]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    simetrica :: Ord a => Rel a -> Bool
-- tal que (simetrica r) se verifica si la relación r es simétrica. Por
-- ejemplo,
--    λ> simetrica (R (fromList [1,3]) (fromList [(1,1),(1,3),(3,1)]))
--    True
--    λ> simetrica (R (fromList [1,3]) (fromList [(1,1),(1,3),(3,2)]))
--    False
--    λ> simetrica (R (fromList [1,3]) (fromList []))
--    True
-- ---------------------------------------------------------------------

simetrica :: Ord a => Rel a -> Bool
simetrica (R _ g) = and [(y,x) `member` g | (x,y) <- elems g]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    subconjunto :: Ord a => Set a -> Set a -> Bool
-- tal que (subconjunto c1 c2) se verifica si c1 es un subconjunto de
-- c2. Por ejemplo,
--    subconjunto (fromList [1,3]) (fromList [3,1,5])  ==  True
--    subconjunto (fromList [3,1,5]) (fromList [1,3])  ==  False
-- ---------------------------------------------------------------------

subconjunto :: Ord a => Set a -> Set a -> Bool
subconjunto = isSubsetOf

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    composicion :: Ord a => Rel a -> Rel a -> Rel a
-- tal que (composicion r s) es la composición de las relaciones r y
-- s. Por ejemplo,
--    λ> let r1 = (R (fromList [1,2]) (fromList [(1,2),(2,2)]))
--    λ> let r2 = (R (fromList [1,2]) (fromList [(2,1)]))
--    λ> let r3 = (R (fromList [1,2]) (fromList [(1,1)]))
--    λ> composicion r1 r2
--    R (fromList [1,2]) (fromList [(1,1),(2,1)])
--    λ> composicion r1 r3
--    R (fromList [1,2]) (fromList [])
-- ---------------------------------------------------------------------

composicion :: Ord a => Rel a -> Rel a -> Rel a
composicion (R u g1) (R _ g2) =
  R u (fromList [(x,z) | (x,y1) <- elems g1,
                         (y2,z) <- elems g2,
                         y1 == y2])

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    transitiva :: Ord a => Rel a -> Bool
-- tal que (transitiva r) se verifica si la relación r es transitiva.
-- Por ejemplo,
--    λ> transitiva (R (fromList [1,3,5])
--                       (fromList [(1,1),(1,3),(3,1),(3,3),(5,5)]))
--    True
--    λ> transitiva (R (fromList [1,3,5])
--                        (fromList [(1,1),(1,3),(3,1),(5,5)]))
--    False
-- ---------------------------------------------------------------------

transitiva :: Ord a => Rel a -> Bool
transitiva r@(R _ g) =
  isSubsetOf (grafo (composicion r r)) g

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    esEquivalencia :: Ord a => Rel a -> Bool
-- tal que (esEquivalencia r) se verifica si la relación r es de
-- equivalencia. Por ejemplo,
--    λ> esEquivalencia (R (fromList [1,3,5])
--                            (fromList [(1,1),(1,3),(3,1),(3,3),(5,5)]))
--    True
--    λ> esEquivalencia (R (fromList [1,2,3,5])
--                            (fromList [(1,1),(1,3),(3,1),(3,3),(5,5)]))
--    False
--    λ> esEquivalencia (R (fromList [1,3,5])
--                            (fromList [(1,1),(1,3),(3,3),(5,5)]))
--    False
-- ---------------------------------------------------------------------

esEquivalencia :: Ord a => Rel a -> Bool
esEquivalencia r = reflexiva r && simetrica r && transitiva r

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    irreflexiva :: Ord a => Rel a -> Bool
-- tal que (irreflexiva r) se verifica si la relación r es irreflexiva;
-- es decir, si ningún elemento de su universo está relacionado con
-- él mismo. Por ejemplo,
--    λ> irreflexiva (R (fromList [1,2,3]) (fromList [(1,2),(2,1),(2,3)]))
--    True
--    λ> irreflexiva (R (fromList [1,2,3]) (fromList [(1,2),(2,1),(3,3)]))
--    False
-- ---------------------------------------------------------------------

irreflexiva :: Ord a => Rel a -> Bool
irreflexiva (R u g) = and [(x,x) `notMember` g | x <- elems u]

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    antisimetrica :: Ord a => Rel a -> Bool
-- tal que (antisimetrica r) se verifica si la relación r es
-- antisimétrica; es decir, si (x,y) e (y,x) están relacionado, entonces
-- x=y. Por ejemplo,
--    antisimetrica (R (fromList [1,2]) (fromList [(1,2)]))       == True
--    antisimetrica (R (fromList [1,2]) (fromList [(1,2),(2,1)])) == False
--    antisimetrica (R (fromList [1,2]) (fromList [(1,1),(2,1)])) == True
-- ---------------------------------------------------------------------

antisimetrica :: Ord a => Rel a -> Bool
antisimetrica (R _ g) =
  [(x,y) | (x,y) <- elems g, x /= y, (y,x) `member` g] == []

-- Otra definición es
antisimetrica2 :: Ord a => Rel a -> Bool
antisimetrica2 (R u g) =
  and [ ((x,y) `member` g && (y,x) `member` g) --> (x == y)
      | x <- elems u, y <- elems u]
  where p --> q = not p || q

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    esTotal :: Ord a => Rel a -> Bool
-- tal que (esTotal r) se verifica si la relación r es total; es decir, si
-- para cualquier par x, y de elementos del universo de r, se tiene que
-- x está relacionado con y ó y está relacionado con x. Por ejemplo,
--    esTotal (R (fromList [1,3]) (fromList [(1,1),(3,1),(3,3)]))  ==  True
--    esTotal (R (fromList [1,3]) (fromList [(1,1),(3,1)]))        ==  False
--    esTotal (R (fromList [1,3]) (fromList [(1,1),(3,3)]))        ==  False
-- ---------------------------------------------------------------------

esTotal :: Ord a => Rel a -> Bool
esTotal (R u g) =
  and [(x,y) `member` g || (y,x) `member` g | x <- xs, y <- xs]
  where xs = elems u

-- ---------------------------------------------------------------------
-- Ejercicio 13. Comprobar con QuickCheck que las relaciones totales son
-- reflexivas.
-- ---------------------------------------------------------------------

prop_total_reflexiva :: Rel Int -> Property
prop_total_reflexiva r =
  esTotal r ==> reflexiva r

-- La comprobación es
--    λ> quickCheck prop_total_reflexiva
--    *** *** Gave up! Passed only 77 tests.

-- ---------------------------------------------------------------------
-- § Clausuras                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    clausuraReflexiva :: Ord a => Rel a -> Rel a
-- tal que (clausuraReflexiva r) es la clausura reflexiva de r; es
-- decir, la menor relación reflexiva que contiene a r. Por ejemplo,
--    λ> clausuraReflexiva (R (fromList [1,3]) (fromList [(1,1),(3,1)]))
--    R (fromList [1,3]) (fromList [(1,1),(3,1),(3,3)])
-- ---------------------------------------------------------------------

clausuraReflexiva :: Ord a => Rel a -> Rel a
clausuraReflexiva (R u g) =
  R u (g `union` fromList [(x,x) | x <- elems u])

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
--    clausuraSimetrica :: Ord a => Rel a -> Rel a
-- tal que (clausuraSimetrica r) es la clausura simétrica de r; es
-- decir, la menor relación simétrica que contiene a r. Por ejemplo,
--    λ> clausuraSimetrica (R (fromList [1,3,5])
--                               (fromList [(1,1),(3,1),(1,5)]))
--    R (fromList [1,3,5]) (fromList [(1,1),(1,3),(1,5),(3,1),(5,1)])
-- ---------------------------------------------------------------------

clausuraSimetrica :: Ord a => Rel a -> Rel a
clausuraSimetrica (R u g) =
  R u (g `union` fromList [(y,x) | (x,y) <- elems g])

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
--    clausuraTransitiva :: Ord a => Rel a -> Rel a
-- tal que (clausuraTransitiva r) es la clausura transitiva de r; es
-- decir, la menor relación transitiva que contiene a r. Por ejemplo,
--    λ> clausuraTransitiva (R (fromList [1..6])
--                                (fromList [(1,2),(2,5),(5,6)]))
--    R (fromList [1,2,3,4,5,6])
--      (fromList [(1,2),(1,5),(1,6),(2,5),(2,6),(5,6)])
-- ---------------------------------------------------------------------

clausuraTransitiva :: Ord a => Rel a -> Rel a
clausuraTransitiva (R u g) = R u (aux g)
  where aux r | cerradoTr r = r
              | otherwise   = aux (r `union` comp r r)
        cerradoTr r = isSubsetOf (comp r r) r
        comp r s    = fromList [(x,z) | (x,y1) <- elems r,
                                        (y2,z) <- elems s,
                                        y1 == y2]

-- ---------------------------------------------------------------------
-- Ejercicio 19. Comprobar con QuickCheck que clausuraTransitiva es
-- transitiva.
-- ---------------------------------------------------------------------

prop_ClausuraTransitiva :: Rel Int -> Bool
prop_ClausuraTransitiva r =
  transitiva (clausuraTransitiva r)

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_ClausuraTransitiva
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Generador de relaciones                                          --
-- ---------------------------------------------------------------------

-- genSet es un generador de relaciones binarias. Por ejemplo,
--    λ> sample genRel
--    (fromList [0],fromList [])
--    (fromList [-1,1],fromList [(-1,1)])
--    (fromList [-3,-2],fromList [])
--    (fromList [-2,0,1,6],fromList [(0,0),(6,0)])
--    (fromList [-7,0,2],fromList [(-7,0),(2,0)])
--    (fromList [2,11],fromList [(2,2),(2,11),(11,2),(11,11)])
--    (fromList [-4,-2,1,4,5],fromList [(1,-2),(1,1),(1,5)])
--    (fromList [-4,-3,-2,6,7],fromList [(-3,-4),(7,-3),(7,-2)])
--    (fromList [-9,-7,0,10],fromList [(10,-9)])
--    (fromList [-10,3,8,10],fromList [(3,3),(10,-10)])
--    (fromList [-10,-9,-7,-6,-5,-4,-2,8,12],fromList [])
genRel :: (Arbitrary a, Integral a) => Gen (Rel a)
genRel = do
  xs <- listOf1 arbitrary
  ys <- listOf (elements [(x,y) | x <- xs, y <- xs])
  return (R (fromList xs) (fromList ys))

instance (Arbitrary a, Integral a) => Arbitrary (Rel a) where
  arbitrary = genRel
