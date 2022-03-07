-- Tablas_y_diccionarios.hs
-- Tablas y diccionarios.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 7-marzo-2022
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- En esta relación se comprueba con QuickCheck cómo las funciones de la
-- tablas definidas en la relación anterior se corresponden con
-- funciones de diccionarios de la libreria Data.Map.

module Tablas_y_diccionarios where

import Prelude hiding (lookup)
import Tablas
import Data.Map.Strict (Map, empty, insert, delete, lookup, mapKeys, alter)
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    tablaAdiccionario :: Ord k => Tabla k v -> Map k v
-- tal que (tablaAdiccionario t) es el diccionario correspondiente a la
-- tabla t. Por ejemplo,
--    λ> tablaAdiccionario (inserta 4 'd' (inserta 2 'a' vacia))
--    fromList [(2,'a'),(4,'d')]
-- ---------------------------------------------------------------------

tablaAdiccionario :: Ord k => Tabla k v -> Map k v
tablaAdiccionario (Tabla xs) =
  foldr (uncurry insert) empty xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir el procedimiento
--    tablaArbitraria :: (Arbitrary k, Arbitrary v) => Gen (Tabla k v)
-- tal que tablaArbitraria es una tabla aleatoria. Por ejemplo,
--    λ> sample (tablaArbitraria :: Gen (Tabla Int Int))
--    Tabla []
--    Tabla [(-3,3),(-2,-4)]
--    Tabla [(5,-2),(3,-9),(6,10),(-2,-1),(10,0),(3,8),(-10,-1),(5,-10),(-7,-1)]
--    Tabla [(-11,-4),(2,2),(6,-2),(11,-3),(-1,-3)]
--    Tabla [(16,7),(15,8),(-6,2),(13,14),(15,2),(4,-10),(17,15),(12,4),(-17,-2)]
--    ...
-- ---------------------------------------------------------------------

tablaArbitraria :: (Arbitrary k, Arbitrary v) => Gen (Tabla k v)
tablaArbitraria = Tabla <$> arbitrary

-- ---------------------------------------------------------------------
-- Ejercicio 3. Declarar Tabla como subclase de Arbitraria usando el
-- generador tablaArbitraria.
-- ---------------------------------------------------------------------

instance (Arbitrary k, Arbitrary v) => Arbitrary (Tabla k v) where
  arbitrary = tablaArbitraria

-- ---------------------------------------------------------------------
-- Ejercicio 4. Comprobar con QuickCheck que la función `inserta` es
-- equivalente a la función `insert` de Data.Map.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_inserta :: Int -> Int -> Tabla Int Int -> Property
prop_inserta n c t =
  tablaAdiccionario (inserta n c t) === insert n c (tablaAdiccionario t)

-- la comprobación es
--    λ> quickCheck prop_inserta
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5. Comprobar con QuickCheck que la función `borra`es
-- equivalente a la función `delete` de Data.Map.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_borra :: Int -> Tabla Int Char -> Property
prop_borra n t =
  tablaAdiccionario (borra n t) === delete n (tablaAdiccionario t)

-- La comprobación es
--    λ> quickCheck prop_borra
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6. Comprobar con QuickCheck que la función `busca`es
-- equivalente a la función `lookup` de Data.Map.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_busca :: Int -> Tabla Int Bool -> Property
prop_busca n t =
  busca n t === lookup n (tablaAdiccionario t)

-- La comprobación es
--    λ> quickCheck prop_busca
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck que la función `aplicaValores`
-- es compatible con la `fmap` de Data.Map.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_aplicaValores :: Fun Char Bool -> Tabla Int Char -> Property
prop_aplicaValores f t =
  tablaAdiccionario (aplicaValores (applyFun f) t)
  === (applyFun f <$> tablaAdiccionario t)

-- La comprobación es
--    λ> quickCheck prop_aplicaValores
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8. Comprobar con QuickCheck que la función `aplicaClaves`
-- es compatible con la `mapKeys` de Data.Map.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_aplicaClaves :: Tabla Int Char -> Property
prop_aplicaClaves t =
  tablaAdiccionario (aplicaClaves succ t)
  === mapKeys succ (tablaAdiccionario t)

-- La comprobación es
--    λ> quickCheck prop_aplicaClaves
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9. Comprobar con QuickCheck que la función `ajusta`
-- es equivalente a la `alter` de Data.Map.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ajusta :: Fun (Maybe Char) (Maybe Char) -> Int -> Tabla Int Char -> Property
prop_ajusta f n t =
  tablaAdiccionario (ajusta (applyFun f) n t)
  === alter (applyFun f) n (tablaAdiccionario t)

-- La comprobación es
--    λ> quickCheck prop_ajusta
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Esta relación de ejercicio es una adaptación de
-- "Tables.hs" https://bit.ly/3Cli8vV de Lars Brünjes.
