{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Definiciones_por_plegados_Spec (main, spec) where

import Definiciones_por_plegados
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "producto" $ do
    it "e1" $
      producto [2,3,5] `shouldBe` 30
    it "e2" $
      producto2 [2,3,5] `shouldBe` 30
    it "e3" $
      producto3 [2,3,5] `shouldBe` 30
    it "e4" $
      producto4 [2,3,5] `shouldBe` 30
    it "e5" $
      producto5 [2,3,5] `shouldBe` 30
    it "e6" $
      producto6 [2,3,5] `shouldBe` 30
    it "p1" $
      property prop_producto
  describe "inversa" $ do
    it "e1" $
      inversa1 [3,2,5]  `shouldBe`  [5,2,3]
    it "e2" $
      inversa2 [3,2,5]  `shouldBe`  [5,2,3]
    it "e3" $
      inversa3 [3,2,5]  `shouldBe`  [5,2,3]
    it "e4" $
      inversa4 [3,2,5]  `shouldBe`  [5,2,3]
    it "e5" $
      inversa5 [3,2,5]  `shouldBe`  [5,2,3]
    it "e6" $
      inversa6 [3,2,5]  `shouldBe`  [5,2,3]
    it "e7" $
      inversa7 [3,2,5]  `shouldBe`  [5,2,3]
    it "p1" $
      property prop_inversa
  describe "coge" $ do
    it "e1" $
      coge 3 "Betis"  `shouldBe`  "Bet"
    it "e2" $
      coge 9 "Betis"  `shouldBe`  "Betis"
    it "p1" $
      property prop_coge
  describe "nFoldl" $ do
    it "e1" $
      nFoldl  (-) 20 [2,5,3]  `shouldBe`  10
    it "e2" $
      nFoldl2 (-) 20 [2,5,3]  `shouldBe`  10
    it "e3" $
      nFoldl3 (-) 20 [2,5,3]  `shouldBe`  10
  describe "foldrArbol" $ do
    it "e1" $
      foldrArbol (+) 0 (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  `shouldBe`  20
    it "e2" $
      foldrArbol (*) 1 (N (N (N (N V 8 V) 2 V) 6 V) 4 V)  `shouldBe`  384
  describe "aplana" $ do
    it "e1" $
      aplana (N (N V 2 V) 5 V)  `shouldBe`  [2,5]
    it "e2" $
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
