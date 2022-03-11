-- Definiciones_por_plegados.hs
-- Definiciones por plegados.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-marzo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Esta relación contiene ejercicios con definiciones por plegado
-- correspondientes al tema 7 que se encuentra en
--    https://jaalonso.github.io/cursos/i1m/temas/tema-7.html
-- Además, se compara con las definiciones recursivas, con acumuladores
-- y con evaluación impaciente. Finalmente, se define la función de
-- plegado para los árboles binarios.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Definiciones_por_plegados where

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Data.List (foldl')
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    producto :: Num a => [a] -> a
-- tal que (producto xs) es el producto de los números de xs. Por
-- ejemplo,
--    producto [2,3,5]  ==  30
-- ---------------------------------------------------------------------

-- 1ª definición
producto :: Num a => [a] -> a
producto []       = 1
producto (x : xs) = x * producto xs

-- 2ª definición
producto2 :: Num a => [a] -> a
producto2 = foldr (*) 1

-- 3ª definición
producto3 :: Num a => [a] -> a
producto3 = aux 1
  where
    aux :: Num a => a -> [a] -> a
    aux r []       = r
    aux r (x : xs) = aux (r * x) xs

-- 4ª definición
producto4 :: Num a => [a] -> a
producto4 = foldl (*) 1

-- 5ª definición
producto5 :: Num a => [a] -> a
producto5 = aux 1
  where
    aux :: Num a => a -> [a] -> a
    aux !r []       = r
    aux !r (x : xs) = aux (r * x) xs

-- 6ª definición
producto6 :: Num a => [a] -> a
producto6 = foldl' (*) 1

-- 7ª definición
producto7 :: Num a => [a] -> a
producto7 = product

-- La propiedad de la equivalencia es
prop_producto :: [Integer] -> Bool
prop_producto xs =
  all (== producto xs)
      [producto2 xs,
       producto3 xs,
       producto4 xs,
       producto5 xs,
       producto6 xs,
       producto7 xs]

-- La comprobación es
--    λ> quickCheck prop_producto
--    +++ OK, passed 100 tests.

-- La comparación de eficiencia es
--    λ> length (show (producto [1..10^5]))
--    456574
--    (8.84 secs, 12,233,346,144 bytes)
--    λ> length (show (producto2 [1..10^5]))
--    456574
--    (8.86 secs, 12,224,409,544 bytes)
--    λ> length (show (producto3 [1..10^5]))
--    456574
--    (8.20 secs, 11,331,830,408 bytes)
--    λ> length (show (producto4 [1..10^5]))
--    456574
--    (8.49 secs, 11,322,997,552 bytes)
--    λ> length (show (producto5 [1..10^5]))
--    456574
--    (1.31 secs, 11,328,586,376 bytes)
--    λ> length (show (producto6 [1..10^5]))
--    456574
--    (1.21 secs, 11,315,687,984 bytes)
--    λ> length (show (producto7 [1..10^5]))
--    456574
--    (8.23 secs, 11,322,997,504 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    inversa :: [a] -> [a]
-- tal que (inversa xs) es la inversa de xs. Por ejemplo,
--    inversa [3,2,5]  ==  [5,2,3]
-- ---------------------------------------------------------------------

-- 1ª definición
inversa1 :: [a] -> [a]
inversa1 []       = []
inversa1 (x : xs) = inversa1 xs ++ [x]

-- 2ª definición
inversa2 :: [a] -> [a]
inversa2 = foldr (\x r -> r ++ [x]) []

-- 3ª definición
inversa3 :: [a] -> [a]
inversa3 = aux []
  where
    aux :: [a] -> [a] -> [a]
    aux r [] = r
    aux r (x : xs) = aux (x : r) xs

-- 4ª definición
inversa4 :: [a] -> [a]
inversa4 = foldl (flip (:)) []

-- 5ª definición
inversa5 :: [a] -> [a]
inversa5 = aux []
  where
    aux :: [a] -> [a] -> [a]
    aux !r [] = r
    aux !r (x : xs) = aux (x : r) xs

-- 6ª definición
inversa6 :: [a] -> [a]
inversa6 = foldl' (flip (:)) []

-- 7ª definición
inversa7 :: [a] -> [a]
inversa7 = reverse

-- La propiedad de equivalencia de las definiciones es
prop_inversa :: [Integer] -> Bool
prop_inversa xs =
  all (== inversa1 xs)
      [inversa2 xs,
       inversa3 xs,
       inversa4 xs,
       inversa5 xs,
       inversa6 xs,
       inversa7 xs]

-- La comprobación es
--    λ> quickCheck prop_inversa
--    +++ OK, passed 100 tests.

-- La comparación de eficiencia es
--    λ> length (inversa1 [1..2*10^4])
--    20000
--    (4.98 secs, 17,512,973,536 bytes)
--    λ> length (inversa2 [1..2*10^4])
--    20000
--    (5.00 secs, 17,511,525,848 bytes)
--    λ> length (inversa3 [1..2*10^4])
--    20000
--    (0.02 secs, 4,342,376 bytes)
--    λ> length (inversa4 [1..2*10^4])
--    20000
--    (0.03 secs, 3,062,336 bytes)
--    λ> length (inversa4 [1..2*10^4])
--    20000
--    (0.03 secs, 3,062,336 bytes)
--    λ> length (inversa6 [1..2*10^4])
--    20000
--    (0.03 secs, 2,262,336 bytes)
--    λ> length (inversa7 [1..2*10^4])
--    20000
--    (0.01 secs, 2,262,336 bytes)
--
--    λ> length (inversa4 [1..5*10^6])
--    5000000
--    (0.74 secs, 680,343,848 bytes)
--    λ> length (inversa5 [1..5*10^6])
--    5000000
--    (1.50 secs, 1,000,343,888 bytes)
--    λ> length (inversa6 [1..5*10^6])
--    5000000
--    (0.51 secs, 480,343,848 bytes)
--    λ> length (inversa7 [1..5*10^6])
--    5000000
--    (0.48 secs, 480,343,848 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    coge :: Int -> [a] -> [a]
-- tal que (coge n xs) es la lista formada por los n primeros elementos
-- de xs. Por ejemplo,
--    coge 3 "Betis"  ==  "Bet"
--    coge 9 "Betis"  ==  "Betis"
-- ---------------------------------------------------------------------

-- 1ª definición
coge :: Int -> [a] -> [a]
coge n _ | n <= 0 = []
coge _ []         = []
coge n (x : xs)   = x : coge (n - 1) xs

-- 2ª definición
coge2 :: Int -> [a] -> [a]
coge2 = flip aux
  where
    aux :: [a] -> Int -> [a]
    aux = foldr f (const [])

    f :: a -> (Int -> [a]) -> Int -> [a]
    f x r n
      | n <= 0    = []
      | otherwise = x : r (n - 1)

-- 3ª definición
coge3 :: Int -> [a] -> [a]
coge3 = take

-- La propiedad de equivalencia de las definiciones es
prop_coge :: Int -> [Int] -> Bool
prop_coge n xs =
  all (== coge n xs)
      [coge2 n xs,
       coge3 n xs]

-- La comprobación es
--    λ> quickCheck prop_coge
--    +++ OK, passed 100 tests.

-- La comparación de eficiencia es
--    λ> length (coge (3*10^6) [1..])
--    3000000
--    (1.85 secs, 984,343,920 bytes)
--    λ> length (coge2 (3*10^6) [1..])
--    3000000
--    (1.76 secs, 1,200,344,192 bytes)
--    λ> length (coge3 (3*10^6) [1..])
--    3000000
--    (0.08 secs, 384,343,800 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    nFoldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
-- tal que (nFoldl f e xs) pliega xs de izquierda a derecha usando el
-- operador f y el valor inicial e. Por ejemplo,
--    nFoldl (-) 20 [2,5,3]  ==  10
-- ---------------------------------------------------------------------

-- 1ª definición
nFoldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
nFoldl _ e []       = e
nFoldl f e (x : xs) = nFoldl f (f e x) xs

-- 2ª definición
nFoldl2 :: forall a b. (b -> a -> b) -> b -> [a] -> b
nFoldl2 f e xs = aux xs e
  where
    aux :: [a] -> b -> b
    aux = foldr g id

    g :: a -> (b -> b) -> b -> b
    g a h b = h (f b a)

-- 3ª definición
nFoldl3 :: forall a b. (b -> a -> b) -> b -> [a] -> b
nFoldl3 = foldl

-- Comparación de eficiencia
--    λ> nFoldl min 0 [1..3*10^6]
--    0
--    (1.62 secs, 778,190,584 bytes)
--    λ> nFoldl2 min 0 [1..3*10^6]
--    0
--    (1.61 secs, 826,190,736 bytes)
--    λ> nFoldl3 min 0 [1..3*10^6]
--    0
--    (1.08 secs, 535,732,976 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Los siguientes árboles  binarios
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
-- Ejercicio 6. Definir el procedimiento
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
-- Ejercicio 7. Declarar Arbol como subclase de Arbitraria usando el
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
-- Ejercicio 8. Definir la función
--    aplana :: Arbol a -> [a]
-- tal que (aplana a) es la lista obtenida aplanando el árbol a. Por
-- ejemplo,
--    aplana (N (N V 2 V) 5 V)  ==  [2,5]
-- ---------------------------------------------------------------------

aplana :: Arbol a -> [a]
aplana V         = []
aplana (N i x d) = aplana i ++ [x] ++ aplana d

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    foldrArbol :: (a -> b -> b) -> b -> Arbol a -> b
-- tal que (foldrArbol f e) pliega el árbol a de derecha a izquierda
-- usando el operador f y el valor inicial e. Por ejemplo,
--    foldrArbol (+) 0 (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  ==  20
--    foldrArbol (*) 1 (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  ==  384
-- ---------------------------------------------------------------------

foldrArbol :: (a -> b -> b) -> b -> Arbol a -> b
foldrArbol f e = foldr f e . aplana

-- ---------------------------------------------------------------------
-- Ejercicio 10. Declarar el tipo Arbol una instancia de la clase
-- Foldable.
-- ---------------------------------------------------------------------

instance Foldable Arbol where
  foldr = foldrArbol

-- ---------------------------------------------------------------------
-- Ejercicio 11. Dado el árbol
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
-- Ejercicio 12. Definir, usando foldr, la función
--    aplana' :: Arbol a -> [a]
-- tal que (aplana' a) es la lista obtenida aplanando el árbol a. Por
-- ejemplo,
--    aplana' (N (N V 2 V) 5 V)  ==  [2,5]
-- ---------------------------------------------------------------------

aplana' :: Arbol a -> [a]
aplana' = foldr (:) []

-- La propiedad es de equivalencia de las definiciones es
prop_aplana :: Arbol Int -> Property
prop_aplana a =
  aplana a === aplana' a

-- La comprobación es
--    λ> quickCheck prop_aplana
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
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
