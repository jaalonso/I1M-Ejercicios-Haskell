{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Relaciones_binarias_homogeneas_Spec (main, spec) where

import Relaciones_binarias_homogeneas
import Test.QuickCheck (property, (==>), Property)
import Data.List (union)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "universo" $
    it "e1" $
      universo r1  `shouldBe`  [1,2,3,4,5,6,7,8,9]
  describe "grafo" $
    it "e1" $
      grafo r1  `shouldBe`  [(1,3),(2,6),(8,9),(2,7)]
  describe "reflexiva" $ do
    it "e1" $
      reflexiva ([1,3],[(1,1),(1,3),(3,3)])    `shouldBe`  True
    it "e2" $
      reflexiva ([1,2,3],[(1,1),(1,3),(3,3)])  `shouldBe`  False
  describe "simetrica" $ do
    it "e1" $
      simetrica ([1,3],[(1,1),(1,3),(3,1)])  `shouldBe`  True
    it "e2" $
      simetrica ([1,3],[(1,1),(1,3),(3,2)])  `shouldBe`  False
    it "e3" $
      simetrica ([1,3],[])                   `shouldBe`  True
  describe "subconjunto" $ do
    it "e1" $
      subconjunto [1,3] [3,1,5]  `shouldBe`  True
    it "e2" $
      subconjunto [3,1,5] [1,3]  `shouldBe`  False
  describe "composicion" $ do
    it "e1" $
      composicion ([1,2],[(1,2),(2,2)]) ([1,2],[(2,1)])
      `shouldBe` ([1,2],[(1,1),(2,1)])
  describe "transitiva" $ do
    it "e1" $
      transitiva ([1,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)])  `shouldBe`  True
    it "e2" $
      transitiva ([1,3,5],[(1,1),(1,3),(3,1),(5,5)])        `shouldBe`  False
  describe "esEquivalencia" $ do
    it "e1" $
      esEquivalencia ([1,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)])
      `shouldBe` True
    it "e2" $
      esEquivalencia ([1,2,3,5],[(1,1),(1,3),(3,1),(3,3),(5,5)])
      `shouldBe` False
    it "e3" $
      esEquivalencia ([1,3,5],[(1,1),(1,3),(3,3),(5,5)])
      `shouldBe` False
  describe "irreflexiva" $ do
    it "e1" $
      irreflexiva ([1,2,3],[(1,2),(2,1),(2,3)])  `shouldBe`  True
    it "e2" $
      irreflexiva ([1,2,3],[(1,2),(2,1),(3,3)])  `shouldBe`  False
  describe "antisimetrica" $ do
    it "e1" $
      antisimetrica ([1,2],[(1,2)])        `shouldBe`  True
    it "e2" $
      antisimetrica ([1,2],[(1,2),(2,1)])  `shouldBe`  False
    it "e3" $
      antisimetrica ([1,2],[(1,1),(2,1)])  `shouldBe`  True
    it "e4" $
      antisimetrica2 ([1,2],[(1,2)])        `shouldBe`  True
    it "e5" $
      antisimetrica2 ([1,2],[(1,2),(2,1)])  `shouldBe`  False
    it "e6" $
      antisimetrica2 ([1,2],[(1,1),(2,1)])  `shouldBe`  True
    it "e7" $
      antisimetrica3 ([1,2],[(1,2)])        `shouldBe`  True
    it "e8" $
      antisimetrica3 ([1,2],[(1,2),(2,1)])  `shouldBe`  False
    it "e9" $
      antisimetrica3 ([1,2],[(1,1),(2,1)])  `shouldBe`  True
  describe "total" $ do
    it "e1" $
      total ([1,3],[(1,1),(3,1),(3,3)])  `shouldBe`  True
    it "e2" $
      total ([1,3],[(1,1),(3,1)])        `shouldBe`  False
    it "e3" $
      total ([1,3],[(1,1),(3,3)])        `shouldBe`  False
  describe "clausuraReflexiva" $ do
    it "e1" $
      clausuraReflexiva ([1,3],[(1,1),(3,1)])
      `shouldBe` ([1,3],[(1,1),(3,1),(3,3)])
    it "e2" $
      property prop_ClausuraReflexiva
  describe "clausuraSimetrica" $ do
    it "e1" $
      clausuraSimetrica ([1,3,5],[(1,1),(3,1),(1,5)])
      `shouldBe` ([1,3,5],[(1,1),(3,1),(1,5),(1,3),(5,1)])
    it "e1" $
      property prop_ClausuraSimetrica
  describe "clausuraTransitiva" $ do
    it "e1" $
      clausuraTransitiva ([1..6],[(1,2),(2,5),(5,6)])
      `shouldBe` ([1,2,3,4,5,6],[(1,2),(2,5),(5,6),(1,5),(2,6),(1,6)])
    it "e2" $
      property prop_ClausuraTransitiva
