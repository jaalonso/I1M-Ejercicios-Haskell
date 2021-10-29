{-# LANGUAGE FlexibleInstances #-}

module Operaciones_con_conjuntos_Spec  (main, spec) where

import Operaciones_con_conjuntos
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "subconjunto1" $ do
    it "e1" $
      subconjunto1 (Cj [2..10]) (Cj [1..10]) `shouldBe` True
    it "e2" $
      subconjunto1 (Cj [1..10]) (Cj [2..10]) `shouldBe` False
    it "e3" $
      subconjunto2 (Cj [2..10]) (Cj [1..10]) `shouldBe` True
    it "e4" $
      subconjunto2 (Cj [1..10]) (Cj [2..10]) `shouldBe` False
    it "e5" $
      subconjunto3 (Cj [2..10]) (Cj [1..10]) `shouldBe` True
    it "e6" $
      subconjunto3 (Cj [1..10]) (Cj [2..10]) `shouldBe` False
    it "e7" $
      subconjunto  (Cj [2..10]) (Cj [1..10]) `shouldBe` True
    it "e8" $
      subconjunto  (Cj [1..10]) (Cj [2..10]) `shouldBe` False
  describe "subconjuntoPropio" $ do
    it "e1" $
      subconjuntoPropio (Cj [2..5]) (Cj [1..7]) `shouldBe` True
    it "e2" $
      subconjuntoPropio (Cj [2..5]) (Cj [1..4]) `shouldBe` False
    it "e3" $
      subconjuntoPropio (Cj [2..5]) (Cj [2..5]) `shouldBe` False
  describe "unitario" $
    it "e1" $
      show (unitario 5) `shouldBe` "{5}"
  describe "cardinal" $ do
    it "e1" $
      cardinal ejConj1 `shouldBe` 7
    it "e1" $
      cardinal ejConj2 `shouldBe` 5
  describe "" $ do
    it "e1" $
      show (union1 ejConj1 ejConj2) `shouldBe` "{0,1,2,3,5,6,7,8,9}"
    it "e2" $
      show (union2 ejConj1 ejConj2) `shouldBe` "{0,1,2,3,5,6,7,8,9}"
    it "e3" $
      show (union  ejConj1 ejConj2) `shouldBe` "{0,1,2,3,5,6,7,8,9}"
  describe "unionG" $ do
    it "e1" $
      show (unionG  [ejConj1, ejConj2]) `shouldBe` "{0,1,2,3,5,6,7,8,9}"
    it "e2" $
      show (unionG2 [ejConj1, ejConj2]) `shouldBe` "{0,1,2,3,5,6,7,8,9}"
  describe "interseccion" $ do
    it "e1" $
      show (interseccion1 (Cj [1..7]) (Cj [4..9])) `shouldBe` "{4,5,6,7}"
    it "e2" $
      show (interseccion2 (Cj [1..7]) (Cj [4..9])) `shouldBe` "{4,5,6,7}"
    it "e3" $
      show (interseccion  (Cj [1..7]) (Cj [4..9])) `shouldBe` "{4,5,6,7}"
  describe "interseccionG" $ do
    it "e1" $
      show (interseccionG  [ejConj1, ejConj2]) `shouldBe` "{1,2,9}"
    it "e2" $
      show (interseccionG2 [ejConj1, ejConj2]) `shouldBe` "{1,2,9}"
  describe "disjuntos" $ do
    it "e1" $
      disjuntos (Cj [2..5]) (Cj [6..9]) `shouldBe` True
    it "e2" $
      disjuntos (Cj [2..5]) (Cj [1..9]) `shouldBe` False
  describe "diferencia" $ do
    it "e1" $
      show (diferencia ejConj1 ejConj2) `shouldBe` "{0,3,5,7}"
    it "e2" $
      show (diferencia ejConj2 ejConj1) `shouldBe` "{6,8}"
  describe "diferenciaSimetrica" $ do
    it "e1" $
      show (diferenciaSimetrica ejConj1 ejConj2) `shouldBe` "{0,3,5,6,7,8}"
    it "e2" $
      show (diferenciaSimetrica ejConj2 ejConj1) `shouldBe` "{0,3,5,6,7,8}"
  describe "filtra" $ do
    it "e1" $
      show (filtra even ejConj1) `shouldBe` "{0,2}"
    it "e2" $
      show (filtra odd  ejConj1) `shouldBe` "{1,3,5,7,9}"
  describe "particion" $
    it "e1" $
      show (particion even ejConj1) `shouldBe` "({0,2},{1,3,5,7,9})"
  describe "divide" $
    it "e1" $
      show (divide 5 ejConj1) `shouldBe` "({0,1,2,3,5},{7,9})"
  describe "mapC" $
    it "e1" $
      show (mapC (*2) (Cj [1..4])) `shouldBe` "{2,4,6,8}"
  describe "everyC" $ do
    it "e1" $
      everyC even (Cj [2,4..10]) `shouldBe` True
    it "e2" $
      everyC even (Cj [2..10])   `shouldBe` False
  describe "someC" $ do
    it "e1" $
      someC even (Cj [1,4,7]) `shouldBe` True
    it "e2" $
      someC even (Cj [1,3,7]) `shouldBe` False
  describe "productoC" $
    it "e1" $
      show (productoC (Cj [1,3]) (Cj [2,4])) `shouldBe` "{(1,2),(1,4),(3,2),(3,4)}"
  describe "potencia" $ do
    it "e1" $
      show (potencia (Cj [1,2]))  `shouldBe` "{{},{1},{1,2},{2}}"
    it "e2" $
      show (potencia (Cj [1..3])) `shouldBe` "{{},{1},{1,2},{1,2,3},{1,3},{2},{2,3},{3}}"
  describe "propiedades" $ do
    it "e1" $
      property propSubconjuntoReflexiva
    it "e2" $
      property propSubconjuntoAntisimetrica2
    it "e3" $
      property propSubconjuntoTransitiva2
    it "e4" $
      property propSubconjuntoVacio
    it "e5" $
      property propUnionIdempotente
    it "e6" $
      property propVacioNeutroUnion
    it "e7" $
      property propUnionCommutativa
    it "e8" $
      property propUnionAsociativa
    it "e9" $
      property propUnionSubconjunto
    it "e10" $
      property propUnionDiferencia
    it "e11" $
      property propInterseccionIdempotente
    it "e12" $
      property propVacioInterseccion
    it "e13" $
      property propInterseccionCommutativa
    it "e14" $
      property propInterseccionAsociativa
    it "e15" $
      property propInterseccionSubconjunto
    it "e16" $
      property propDistributivaIU
    it "e17" $
      property propDistributivaUI
    it "e18" $
      property propDiferenciaVacio2
    it "e19" $
      property propDiferenciaVacio2
    it "e20" $
      property propDiferenciaDif1
    it "e21" $
      property propDiferenciaDif2
    it "e22" $
      property propDiferenciaSubc
    it "e23" $
      property propDiferenciaDisj
    it "e24" $
      property propDiferenciaUI
