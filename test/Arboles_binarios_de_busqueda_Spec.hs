{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Arboles_binarios_de_busqueda_Spec (main, spec) where

import Arboles_binarios_de_busqueda
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mapArbol" $ do
    it "e1" $
      mapArbol (+1) (N V 7 (N V 8 V))  `shouldBe`  N V 8 (N V 9 V)
    it "p1" $
      property prop_mapArbol
  describe "aplana" $
    it "e1" $
      aplana (N (N V 2 V) 5 V)  `shouldBe`  [2,5]
  describe "estrictamenteCreciente" $ do
    it "e1" $
      estrictamenteCreciente [2,3,5]  `shouldBe`  True
    it "e1" $
      estrictamenteCreciente [2,3,3]  `shouldBe`  False
    it "e1" $
      estrictamenteCreciente [2,5,3]  `shouldBe`  False
  describe "esABB" $ do
    it "e1" $
      esABB (N (N V 2 V) 5 V)  `shouldBe`  True
    it "e2" $
      esABB (N V 3 (N V 3 V))  `shouldBe`  False
    it "p1" $
      property prop_esABB
  describe "pertenece" $ do
    it "e1" $
      pertenece 5 (N (N V 2 V) 5 V)  `shouldBe`  True
    it "e2" $
      pertenece 3 (N (N V 2 V) 5 V)  `shouldBe`  False
  describe "minABB" $ do
    it "e1" $
      minABB (N (N V (-1) (N V 0 (N V 9 V))) 10 V)  `shouldBe`  Just (-1)
    it "e2" $
      minABB V                                      `shouldBe`  (Nothing :: Maybe Int)
  describe "maxABB" $ do
    it "e1" $
      maxABB (N (N V (-1) (N V 0 (N V 9 V))) 10 V)  `shouldBe`  Just 10
    it "e2" $
      maxABB V                                      `shouldBe`  (Nothing :: Maybe Int)
  describe "abbArbitrario" $
    it "p1" $
      property prop_abbArbitrario_esABB
  describe "inserta" $ do
    it "e1" $
      inserta 7 (N (N V (-1) (N V 0 (N V 9 V))) 10 V)
      `shouldBe` N (N V (-1) (N V 0 (N (N V 7 V) 9 V))) 10 V
    it "p1" $
      property prop_inserta_ABB
    it "p2" $
      property prop_inserta_idempotente
    it "p3" $
      property prop_pertenece_inserta
  describe "borra" $ do
    it "e1" $
      borra 1 (N (N V 1 V) 2 (N V 7 V))  `shouldBe`  N V 2 (N V 7 V)
    it "e2" $
      borra 2 (N (N V 1 V) 2 (N V 7 V))  `shouldBe`  N V 1 (N V 7 V)
    it "e3" $
      borra 7 (N (N V 1 V) 2 (N V 7 V))  `shouldBe`  N (N V 1 V) 2 V
    it "e4" $
      borra 8 (N (N V 1 V) 2 (N V 7 V))  `shouldBe`  N (N V 1 V) 2 (N V 7 V)
    it "p1" $
      property prop_borra_ABB
    it "p2" $
      property prop_pertenece_borra
  describe "ordenadaDescendente" $ do
    it "e1" $
      ordenadaDescendente [3,2,5]  `shouldBe`  [5,3,2]
    it "e2" $
      ordenadaDescendente1 [3,2,5]  `shouldBe`  [5,3,2]
    it "p1" $
      property prop_ordenadaDescendente
  describe "listaAabb" $ do
    it "e1" $
      listaAabb [5,1,2,4,3]
      `shouldBe` N (N (N V 1 V) 2 V) 3 (N V 4 (N V 5 V))
    it "p1" $
      property prop_listaAabb_esABB
    it "p2" $
      property prop_listaAabb_ordena
  describe "etiquetaArbol" $
    it "e1" $
      etiquetaArbol (N (N (N (N V 8 V) 4 V) 5 V) 7 V) "Betis"
      `shouldBe` N (N (N (N V (8,'B') V) (4,'e') V) (5,'t') V) (7,'i') V
  describe "enumeraArbol" $ do
    it "e1" $
      enumeraArbol (N (N (N (N V 8 V) 4 V) 5 V) 7 V)
      `shouldBe` N (N (N (N V (8,1) V) (4,2) V) (5,3) V) (7,4) V
    it "e2" $
      enumeraArbol' (N (N (N (N V 8 V) 4 V) 5 V) 7 V)
      `shouldBe` N (N (N (N V (8,1) V) (4,2) V) (5,3) V) (7,4) V
    it "p1" $
      property prop_enumeraArbol
  describe "foldrArbol" $ do
    it "e1" $
      foldrArbol (+) 0 (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  `shouldBe`  20
    it "e2" $
      foldrArbol (*) 1 (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  `shouldBe`  384
  describe "aplana'" $ do
    it "e1" $
      aplana' (N (N V 2 V) 5 V) `shouldBe`  [2,5]
    it "p1" $
      property prop_aplana
  describe "todos" $ do
    it "e1" $
      todos even [2,6,4]  `shouldBe`  True
    it "e2" $
      todos even [2,5,4]  `shouldBe`  False
    it "e3" $
      todos even (Just 6) `shouldBe`  True
    it "e4" $
      todos even (Just 5) `shouldBe`  False
    it "e5" $
      todos even Nothing  `shouldBe`  True
    it "e6" $
      todos even (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  `shouldBe`  True
    it "e7" $
      todos even (N (N (N (N V 8 V) 5 V) 6 V) 4 V)  `shouldBe`  False
