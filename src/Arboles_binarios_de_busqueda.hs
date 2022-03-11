-- Arboles_binarios_de_busqueda.hs
-- Árboles binarios de búsqueda.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación de ejercicios se definen los tipos de datos de los
-- árboles binarios y los árboles binarios de búsqueda, se definen
-- funciones sobre dichos tipos y se comprueban con QuickCheck
-- propiedades de las funciones definidas.

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Arboles_binarios_de_busqueda where

import Data.List (nub, sort, sortBy)
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import qualified Control.Monad.State as S

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los siguientes árboles  binarios
--         9                9
--        / \              /
--       /   \            /
--      8     6          8
--     / \   / \        / \
--    3   2 4   5      3   2
-- se pueden representar por los términos
--    N 9 (N 8 (N 3 V V) (N 2 V V)) (N 6 (N 4 V V) (N 5 V V))
--    N 9 (N 8 (N 3 V V) (N 2 V V)) V
-- usando los contructores N (para los nodos) y V (para los árboles
-- vacío).
--
-- Definir el tipo de datos Arbol correspondiente a los términos
-- anteriores.
-- ---------------------------------------------------------------------

data Arbol a = N (Arbol a) a (Arbol a)
             | V
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir el procedimiento
--    arbolArbitrario :: Arbitrary a => Int -> Gen (Arbol a)
-- tal que (arbolArbitrario n) es un árbol aleatorio de altura n. Por
-- ejemplo,
--    λ> sample (arbolArbitrario 3 :: Gen (Arbol Int))
--    N (N V 0 V) 0 V
--    N (N (N (N (N (N V 1 V) (-1) V) (-2) V) (-1) V) 0 V) (-2) V
--    N (N (N V (-4) V) (-4) V) 3 V
--    N (N (N (N (N V (-5) V) 5 V) (-2) V) 4 V) (-1) V
--    N (N (N V 5 V) 1 V) (-2) V
--    N (N (N (N (N (N (N (N V 3 V) 10 V) 6 V) (-2) V) (-1) V) 6 V) 3 V) 6 V
--    N (N V 10 V) 3 V
--    N (N V 1 V) (-14) V
--    N (N (N (N (N (N V 9 V) 15 V) 14 V) (-8) V) (-1) V) (-11) V
--    N (N (N (N V (-8) V) 4 V) (-14) V) (-10) V
--    N (N V (-13) V) 0 V
-- ---------------------------------------------------------------------

arbolArbitrario :: Arbitrary a => Int -> Gen (Arbol a)
arbolArbitrario n
  | n <= 1    = return V
  | otherwise = do
      k <- choose (2, n - 1)
      N <$> arbolArbitrario k <*> arbitrary <*> arbolArbitrario (n - k)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Declarar Arbol como subclase de Arbitraria usando el
-- generador arbolArbitrario.
-- ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbolArbitrario
  shrink V   = []
  shrink (N i x d) = i :
                     d :
                     [N i' x  d  | i' <- shrink i] ++
                     [N i  x' d  | x' <- shrink x] ++
                     [N i  x  d' | d' <- shrink d]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    mapArbol :: (a -> b) -> Arbol a -> Arbol b
-- tal que (mapArbol f a) es el árbol obtenido aplicando la función f a
-- los elementos del árbol a. Por ejemplo,
--    mapArbol (+1) (N V 7 (N V 8 V))  ==  N V 8 (N V 9 V)
-- ---------------------------------------------------------------------

mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol _ V         = V
mapArbol f (N i x d) = N (mapArbol f i) (f x) (mapArbol f d)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Comprobar con QuickCheck que, para todo árbol a,
--   map id a == a
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mapArbol :: Arbol Int -> Property
prop_mapArbol t =
  mapArbol id t === t

-- La comprobación es
--    λ> quickCheck prop_mapArbol
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6. Declarar el tipo Arbol una instancia de la clase
-- Functor.
-- ---------------------------------------------------------------------

instance Functor Arbol where
  fmap = mapArbol

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    aplana :: Arbol a -> [a]
-- tal que (aplana a) es la lista obtenida aplanando el árbol a. Por
-- ejemplo,
--    aplana (N (N V 2 V) 5 V)  ==  [2,5]
-- ---------------------------------------------------------------------

aplana :: Arbol a -> [a]
aplana V         = []
aplana (N i x d) = aplana i ++ [x] ++ aplana d

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    estrictamenteCreciente :: Ord a => [a] -> Bool
-- tal que (estrictamenteCreciente xs) se verifica si xs es
-- estrictamente creciente. Por ejemplo,
--    estrictamenteCreciente [2,3,5]  ==  True
--    estrictamenteCreciente [2,3,3]  ==  False
--    estrictamenteCreciente [2,5,3]  ==  False
-- ---------------------------------------------------------------------

estrictamenteCreciente :: Ord a => [a] -> Bool
estrictamenteCreciente [] = True
estrictamenteCreciente [_] = True
estrictamenteCreciente (x : y : ys) = x < y && estrictamenteCreciente (y : ys)

-- ---------------------------------------------------------------------
-- Ejercicio 9. Un árbol binario de búsqueda (ABB) es un árbol binario
-- tal que el valor de cada nodo es mayor que los valores de su subárbol
-- izquierdo y es menor que los valores de su subárbol derecho y,
-- además, ambos subárboles son árboles binarios de búsqueda. Por
-- ejemplo, al almacenar los valores de [2,3,4,5,6,8,9] en un ABB se
-- puede obtener los siguientes ABB:
--
--       5                     5
--      / \                   / \
--     /   \                 /   \
--    2     6               3     8
--     \     \             / \   / \
--      4     8           2   4 6   9
--     /       \
--    3         9
--
-- El objetivo principal de los ABB es reducir el tiempo de acceso a los
-- valores.
--
-- Definir la función
--    esABB :: Ord a => Arbol a -> Bool
-- tal que (esABB a) se verifica si a es un árbol binario de
-- búsqueda. Por ejemplo,
--    esABB (N (N V 2 V) 5 V)  ==  True
--    esABB (N V 3 (N V 3 V))  ==  False
-- ---------------------------------------------------------------------

esABB :: Ord a => Arbol a -> Bool
esABB = estrictamenteCreciente . aplana

-- ---------------------------------------------------------------------
-- Ejercicio 10. Comprobar con QuickCheck que un árbol binario a es un
-- ABB si, y solo si, (aplana a) es una lista ordenada sin elementos
-- repetidos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_esABB :: Arbol Int -> Property
prop_esABB a =
  esABB a === (xs == sort (nub xs))
  where xs = aplana a

-- La comprobación es
--    λ> quickCheck prop_esABB
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    minABB :: ABB a -> Maybe a
-- tal que (minABB a) es el mínimo del ABB a. Por ejemplo,
--    minABB (N (N V (-1) (N V 0 (N V 9 V))) 10 V)  ==  Just (-1)
--    minABB V                                      ==  Nothing
-- ---------------------------------------------------------------------

minABB :: ABB a -> Maybe a
minABB V = Nothing
minABB (N i x _) =
  case minABB i of
    Nothing -> Just x
    Just y  -> Just y

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    maxABB :: ABB a -> Maybe a
-- tal que (maxABB a) es el máximo del ABB a. Por ejemplo,
--    maxABB (N (N V (-1) (N V 0 (N V 9 V))) 10 V)  ==  Just 10
--    maxABB V                                      ==  Nothing
-- ---------------------------------------------------------------------

maxABB :: ABB a -> Maybe a
maxABB V = Nothing
maxABB (N _ x d) =
  case maxABB d of
    Nothing -> Just x
    Just y  -> Just y

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir el tipo ABB para los árboles binarios de
-- búsqueda (aunque el sistema de tipo no lo compruebe).
-- ---------------------------------------------------------------------

type ABB a = Arbol a

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir el tipo ABB' con el constructor ABB' para los
-- árboles binarios de búsqueda.
-- ---------------------------------------------------------------------

newtype ABB' = ABB' (ABB Integer)
  deriving newtype Show

-- ---------------------------------------------------------------------
-- Ejercicio 15. Definir el procedimiento
--    abbArbitrario :: Int -> Gen (Arbol a)
-- tal que (abbArbitrario n) es un árbol binario de búsqueda aleatorio
-- de altura n. Por ejemplo,
--    λ> sample (abbArbitrario 4)
--    N (N V (-2) (N V (-1) V)) 0 V
--    N (N (N V (-2) V) 0 V) 1 V
--    N (N V (-4) V) (-3) (N V (-2) V)
--    N (N V (-1) V) 3 (N V 4 V)
--    N V 2 (N (N V 3 V) 4 V)
--    N (N V (-8) V) (-7) (N V (-2) V)
--    N (N V 1 (N V 6 V)) 11 V
--    N (N (N V (-21) V) (-12) V) (-11) V
--    N V (-1) (N V 0 (N V 1 V))
--    N (N V (-16) (N V (-15) V)) (-11) V
--    N (N (N V (-6) V) (-5) V) (-4) V
-- ---------------------------------------------------------------------

abbArbitrario :: Int -> Gen ABB'
abbArbitrario n
  | n <= 1    = return (ABB' V)
  | otherwise = do
      ni <- choose (1, n - 1)
      let nd = n - ni
      x <- arbitrary
      ABB' i' <- abbArbitrario ni
      ABB' d' <- abbArbitrario nd
      let iMax   = fromMaybe (x - 1) (maxABB i')
          dMin   = fromMaybe (x + 1) (minABB d')
          iDelta = max 0 (iMax - x + 1)
          dDelta = max 0 (x - dMin + 1)
          i      = (+ (- iDelta)) <$> i'
          d      = (+ dDelta) <$> d'
      return (ABB' (N i x d))

-- ---------------------------------------------------------------------
-- Ejercicio 16. Declarar ABB' como subclase de Arbitraria usando el
-- generador abbArbitrario.
-- ---------------------------------------------------------------------

instance Arbitrary ABB' where
  arbitrary = sized abbArbitrario
  shrink (ABB' V) = []
  shrink (ABB' (N i x d)) =
    ABB' i :
    ABB' d :
    [ABB' (N i' x d) | ABB' i' <- shrink (ABB' i)] ++
    [ABB' (N i x d') | ABB' d' <- shrink (ABB' d)]

-- ---------------------------------------------------------------------
-- Ejercicio 17. Comprobar con QuickCheck que abbArbitrario genera
-- árboles binarios de búsqueda.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_abbArbitrario_esABB :: ABB' -> Bool
prop_abbArbitrario_esABB (ABB' a) = esABB a

-- La comprobación es
--    λ> quickCheck prop_abbArbitrario_esABB
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    pertenece :: Ord a => a -> ABB a -> Bool
-- tal que (pertenece x a) se verifica si x pertenece al ABB a. Por
-- ejemplo,
--    pertenece 5 (N (N V 2 V) 5 V)  ==  True
--    pertenece 3 (N (N V 2 V) 5 V)  ==  False
-- ---------------------------------------------------------------------

pertenece :: Ord a => a -> ABB a -> Bool
pertenece _ V = False
pertenece x (N i y d)
  | x < y     = pertenece x i
  | x > y     = pertenece x d
  | otherwise = True

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    inserta :: Ord a => a -> ABB a -> ABB a
-- tal que (inserta x a) es el ABB obtenido insertando x en a. Por
-- ejemplo,
--    λ> inserta 7 (N (N V (-1) (N V 0 (N V 9 V))) 10 V)
--    N (N V (-1) (N V 0 (N (N V 7 V) 9 V))) 10 V
-- ---------------------------------------------------------------------

inserta :: Ord a => a -> ABB a -> ABB a
inserta x V = N V x V
inserta x (N l y r)
  | x < y     = N (inserta x l) y r
  | x > y     = N l y (inserta x r)
  | otherwise = N l x r

-- ---------------------------------------------------------------------
-- Ejercicio 20. Comprobar con QuickCheck que si a es un ABB, entonces
-- (inserta x a) es un ABB.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_inserta_ABB :: Integer -> ABB' -> Bool
prop_inserta_ABB x (ABB' a) =
  esABB (inserta x a)

-- La comprobación es
--    λ> quickCheck prop_inserta_ABB
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 21. Comprobar con QuickCheck que inserta es idempotente.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_inserta_idempotente :: Integer -> ABB' -> Property
prop_inserta_idempotente x (ABB' a) =
  inserta x (inserta x a) === inserta x a

-- La comprobación es
--    λ> quickCheck prop_inserta_idempotente
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 22. Comprobar con QuickCheck que x pertenece a (inserta x a).
-- ---------------------------------------------------------------------

-- La propiedad es
prop_pertenece_inserta :: Integer -> ABB' -> Bool
prop_pertenece_inserta x (ABB' a) =
  x `pertenece` inserta x a

-- La comprobación es
--    λ> quickCheck prop_pertenece_inserta
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    borra :: Ord a => a -> ABB a -> ABB a
-- tal que (borra x a) es el árbol binario de búsqueda obtenido borando
-- en a el elemento x. Por ejemplo,
--    borra 1 (N (N V 1 V) 2 (N V 7 V))  ==  N V 2 (N V 7 V)
--    borra 2 (N (N V 1 V) 2 (N V 7 V))  ==  N V 1 (N V 7 V)
--    borra 7 (N (N V 1 V) 2 (N V 7 V))  ==  N (N V 1 V) 2 V
--    borra 8 (N (N V 1 V) 2 (N V 7 V))  ==  N (N V 1 V) 2 (N V 7 V)
-- ---------------------------------------------------------------------

borra :: Ord a => a -> ABB a -> ABB a
borra _ V = V
borra x (N i y d)
  | x < y     = N (borra x i) y d
  | x > y     = N i y (borra x d)
  | otherwise = case maxABB i of
                  Nothing -> d
                  Just z  -> N (borra z i) z d

-- ---------------------------------------------------------------------
-- Ejercicio 24. Comprobar con QuickCheck que si a es un ABB, entonces
-- (borra x a) es un ABB.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_borra_ABB :: Integer -> ABB' -> Bool
prop_borra_ABB x (ABB' a) =
  esABB (borra x a)

-- La comprobación es
--    λ> quickCheck prop_borra_ABB
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 25. Comprobar con QuickCheck que si a es un ABB, entonces
-- x no pertenece a (borra x a).
-- ---------------------------------------------------------------------

-- La propiedad es
prop_pertenece_borra :: Integer -> ABB' -> Bool
prop_pertenece_borra x (ABB' a) =
  not (pertenece x (borra x a))

-- La comprobación es
--    λ> quickCheck prop_pertenece_borra
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--    ordenadaDescendente :: Ord a => [a] -> [a]
-- tal que (ordenadaDescendente xs) es la lista obtenida ordenando xs de
-- manera descendente. Por ejemplo,
--    ordenadaDescendente [3,2,5]  ==  [5,3,2]
-- ---------------------------------------------------------------------

-- 1ª definición
ordenadaDescendente1 :: Ord a => [a] -> [a]
ordenadaDescendente1 = reverse . sort

-- 2ª definición
ordenadaDescendente :: Ord a => [a] -> [a]
ordenadaDescendente = sortBy (flip compare)

-- Comprobación de la equivalencia
-- ===============================

-- La propiedad es
prop_ordenadaDescendente :: [Int] -> Property
prop_ordenadaDescendente xs =
  ordenadaDescendente1 xs === ordenadaDescendente xs

-- La comprobación es
--    λ> quickCheck prop_ordenadaDescendente
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (ordenadaDescendente1 [1..7*10^6])
--    1
--    (1.54 secs, 1,008,339,840 bytes)
--    λ> last (ordenadaDescendente [1..7*10^6])
--    1
--    (0.85 secs, 672,339,848 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--    listaAabb :: Ord a => [a] -> ABB a
-- tal que (listaAabb xs) es un árbol binario de búsqueda cuyos
-- elementos son los de xs. Por ejemplo,
--    λ> listaAabb [5,1,2,4,3]
--    N (N (N V 1 V) 2 V) 3 (N V 4 (N V 5 V))
-- ---------------------------------------------------------------------

listaAabb :: Ord a => [a] -> ABB a
listaAabb = foldr inserta V

-- ---------------------------------------------------------------------
-- Ejercicio 28. Comprobar con QuickCheck que, para toda lista xs,
-- (listaAabb xs) esun árbol binario de búsqueda.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_listaAabb_esABB :: [Int] -> Bool
prop_listaAabb_esABB xs = esABB (listaAabb xs)

-- La comprobación es
--    λ> quickCheck prop_listaAabb_esABB
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 29. Comprobar con QuickCheck que, para toda lista xs,
-- aplana (listaAabb xs) es la lista ordenada de los elementos de xs sin
-- repeticiones.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_listaAabb_ordena :: [Int] -> Property
prop_listaAabb_ordena xs =
  aplana (listaAabb xs) === nub (sort xs)

-- La comprobación es
--    λ> quickCheck prop_listaAabb_ordena
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--    etiquetaArbol :: Arbol a -> [b] -> Arbol (a, b)
-- tal que (etiquetaArbolt xs) es el árbol t con las hojas etiquetadas
-- con elementos de xs. Por ejemplo,
--    λ> etiquetaArbol (N (N (N (N V 8 V) 4 V) 5 V) 7 V) "Betis"
--    N (N (N (N V (8,'B') V) (4,'e') V) (5,'t') V) (7,'i') V
-- ---------------------------------------------------------------------

etiquetaArbol :: Arbol a -> [b] -> Arbol (a, b)
etiquetaArbol t xs = fst (aux xs t)
  where
    aux :: [b] -> Arbol a -> (Arbol (a, b), [b])
    aux ys V         = (V, ys)
    aux ys (N i x d) = (N i' (x, b) d', ys'')
      where (i', b : ys') = aux ys i
            (d', ys'')    = aux ys' d

-- ---------------------------------------------------------------------
-- Ejercicio 31. Definir la función
--    enumeraArbol :: Arbol a -> Arbol (a, Int)
-- tal que (enumeraArbolt xs) es el árbol t con las hojas enumeradas
-- por números crecientes de izquierda a derecha. Por ejemplo,
--    λ> enumeraArbol (N (N (N (N V 8 V) 4 V) 5 V) 7 V)
--    N (N (N (N V (8,1) V) (4,2) V) (5,3) V) (7,4) V
-- ---------------------------------------------------------------------

enumeraArbol :: Arbol a -> Arbol (a, Int)
enumeraArbol = flip etiquetaArbol [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 32. Definir la función
--    traverseArbol :: Applicative f => (a -> f b) -> Arbol a -> f (Arbol b)
-- tal que (traverseArbol f a) aplica a cada elemento de a la acción f,
-- las acciones las evalúa de izquierda a derecha y recolexta los
-- resultados. Por ejemplo,
--    λ> dec n x = if x > n then Just (x - 1) else Nothing
--    λ> traverseArbol (dec 3) (N (N (N (N V 8 V) 4 V) 5 V) 7 V)
--    Just (N (N (N (N V 7 V) 3 V) 4 V) 6 V)
--    λ> traverseArbol (dec 4) (N (N (N (N V 8 V) 4 V) 5 V) 7 V)
--    Nothing
-- ---------------------------------------------------------------------

traverseArbol :: Applicative f => (a -> f b) -> Arbol a -> f (Arbol b)
traverseArbol _ V         = pure V
traverseArbol f (N l x r) = N <$> traverseArbol f l <*> f x <*> traverseArbol f r

-- ---------------------------------------------------------------------
-- Ejercicio 33. Definir, usando traverseArbol, la función
--    enumeraArbol' :: Arbol a -> Arbol (a, Int)
-- que sea equivalente a enumeraArbol. Por ejemplo,
--    λ> dec n x = if x > n then Just (x - 1) else Nothing
--    λ> traverseArbol' (dec 3) (N (N (N (N V 8 V) 4 V) 5 V) 7 V)
--    Just (N (N (N (N V 7 V) 3 V) 4 V) 6 V)
--    λ> traverseArbol' (dec 4) (N (N (N (N V 8 V) 4 V) 5 V) 7 V)
--    Nothing
-- ---------------------------------------------------------------------

enumeraArbol' :: Arbol a -> Arbol (a, Int)
enumeraArbol' t = S.evalState (traverseArbol f t) 1
  where
    f c = do
      l <- S.get
      S.put $ succ l
      return (c, l)

-- ---------------------------------------------------------------------
-- Ejercicio 34. Comprobar con QuickCheck que las funciones enumeraArbol
-- y enumeraArbol' son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_enumeraArbol :: Arbol Char -> Property
prop_enumeraArbol a =
  enumeraArbol a === enumeraArbol' a

-- La comprobación es
--    λ> quickCheck prop_enumeraArbol
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 35. Definir la función
--    foldrArbol :: (a -> b -> b) -> b -> Arbol a -> b
-- tal que (foldrArbol f e) pliega el árbol a de derecha a izquierda
-- usando el operador f y el valor inicial e. Por ejemplo,
--    foldrArbol (+) 0 (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  ==  20
--    foldrArbol (*) 1 (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  ==  384
-- ---------------------------------------------------------------------

foldrArbol :: (a -> b -> b) -> b -> Arbol a -> b
foldrArbol f e = foldr f e . aplana

-- ---------------------------------------------------------------------
-- Ejercicio 36. Declarar el tipo Arbol una instancia de la clase
-- Foldable.
-- ---------------------------------------------------------------------

instance Foldable Arbol where
  foldr = foldrArbol

-- ---------------------------------------------------------------------
-- Ejercicio 37. Dado el árbol
--    a = N (N (N (N V 8 V) 2 V) 6 V) 4 V
-- Calcular su longitud, máximo, mínimo, suma, producto y lista de
-- elementos.
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> a = N (N (N (N V 8 V) 2 V) 6 V) 4 V
--    λ> length a
--    4
--    λ> maximum a
--    8
--    λ> minimum a
--    2
--    λ> sum a
--    20
--    λ> product a
--    384
--    λ> Data.Foldable.toList a
--    [8,2,6,4]

-- ---------------------------------------------------------------------
-- Ejercicio 38. Definir, usando foldr, la función
--    aplana' :: Arbol a -> [a]
-- tal que (aplana a) es la lista obtenida aplanando el árbol a. Por
-- ejemplo,
--    aplana' (N (N V 2 V) 5 V)  ==  [2,5]
-- ---------------------------------------------------------------------

aplana' :: Arbol a -> [a]
aplana' = foldr (:) []

-- ---------------------------------------------------------------------
-- Ejercicio 39. Comprobar con QuickCheck que las funciones aplana y
-- aplana' son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_aplana :: Arbol Int -> Property
prop_aplana a =
  aplana a === aplana' a

-- La comprobación es
--    λ> quickCheck prop_aplana
--    +++ OK, passed 100 tests.


-- ---------------------------------------------------------------------
-- Ejercicio 30. Definir la función
--    todos :: Foldable t => (a -> Bool) -> t a -> Bool
-- tal que (todos p xs) se verifica si todos los elementos de xs cumplen
-- la propiedad p. Por ejemplo,
--    todos even [2,6,4]  ==  True
--    todos even [2,5,4]  ==  False
--    todos even (Just 6) ==  True
--    todos even (Just 5) ==  False
--    todos even Nothing  ==  True
--    todos even (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  ==  True
--    todos even (N (N (N (N V 8 V) 5 V) 6 V) 4 V)  ==  False
-- ---------------------------------------------------------------------

todos :: Foldable t => (a -> Bool) -> t a -> Bool
todos p = foldr (\x b -> p x && b) True
