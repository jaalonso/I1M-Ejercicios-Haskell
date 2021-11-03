module Relaciones_binarias_homogeneas_con_la_libreria_Spec (main, spec) where

import Relaciones_binarias_homogeneas_con_la_libreria
import Test.QuickCheck
import Data.Set as S
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)

main :: IO ()
main = hspec spec

spec :: Spec
spec =   do
  describe "universo" $
    it "e1" $
      show (universo r1) `shouldBe` "fromList [1,2,3,4,5,6,7,8,9]"
  describe "grafo" $
    it "e1" $
      show (grafo r1) `shouldBe` "fromList [(1,3),(2,6),(2,7),(8,9)]"
  describe "reflexiva" $ do
    it "e1" $
      reflexiva (R (fromList [1,3]) (fromList [(1,1),(1,3),(3,3)]))
      `shouldBe` True
    it "e2" $
      reflexiva (R (fromList [1,2,3]) (fromList [(1,1),(1,3),(3,3)]))
      `shouldBe` False
  describe "simetrica" $ do
    it "e1" $
      simetrica (R (fromList [1,3]) (fromList [(1,1),(1,3),(3,1)]))
      `shouldBe` True
    it "e2" $
      simetrica (R (fromList [1,3]) (fromList [(1,1),(1,3),(3,2)]))
      `shouldBe` False
    it "e3" $
      simetrica (R (fromList [1,3]) (fromList []))
      `shouldBe` True
  describe "subconjunto" $ do
    it "e1" $
      subconjunto (fromList [1,3]) (fromList [3,1,5])  `shouldBe`  True
    it "e2" $
      subconjunto (fromList [3,1,5]) (fromList [1,3])  `shouldBe`  False
  describe "composicion" $ do
    let r1' = (R (fromList [1,2]) (fromList [(1,2),(2,2)]))
        r2' = (R (fromList [1,2]) (fromList [(2,1)]))
        r3' = (R (fromList [1,2]) (fromList [(1,1)]))
    it "e1" $
      show (composicion r1' r2')
      `shouldBe` "R (fromList [1,2]) (fromList [(1,1),(2,1)])"
    it "e2" $
      show (composicion r1' r3')
      `shouldBe` "R (fromList [1,2]) (fromList [])"
  describe "transitiva" $ do
    it "e1" $
      transitiva (R (fromList [1,3,5]) (fromList [(1,1),(1,3),(3,1),(3,3),(5,5)]))
      `shouldBe` True
    it "e2" $
      transitiva (R (fromList [1,3,5]) (fromList [(1,1),(1,3),(3,1),(5,5)]))
      `shouldBe` False
  describe "esEquivalencia" $ do
    it "e1" $
      esEquivalencia (R (fromList [1,3,5]) (fromList [(1,1),(1,3),(3,1),(3,3),(5,5)]))
      `shouldBe` True
    it "e2" $
      esEquivalencia (R (fromList [1,2,3,5]) (fromList [(1,1),(1,3),(3,1),(3,3),(5,5)]))
      `shouldBe` False
    it "e3" $
      esEquivalencia (R (fromList [1,3,5]) (fromList [(1,1),(1,3),(3,3),(5,5)]))
      `shouldBe` False
  describe "irreflexiva" $ do
    it "e1" $
      irreflexiva (R (fromList [1,2,3]) (fromList [(1,2),(2,1),(2,3)]))
      `shouldBe` True
    it "e2" $
      irreflexiva (R (fromList [1,2,3]) (fromList [(1,2),(2,1),(3,3)]))
      `shouldBe` False
  describe "antisimetrica" $ do
    it "e1" $
      antisimetrica (R (fromList [1,2]) (fromList [(1,2)]))       `shouldBe` True
    it "e2" $
      antisimetrica (R (fromList [1,2]) (fromList [(1,2),(2,1)])) `shouldBe` False
    it "e3" $
      antisimetrica (R (fromList [1,2]) (fromList [(1,1),(2,1)])) `shouldBe` True
    it "e4" $
      antisimetrica2 (R (fromList [1,2]) (fromList [(1,2)]))       `shouldBe` True
    it "e5" $
      antisimetrica2 (R (fromList [1,2]) (fromList [(1,2),(2,1)])) `shouldBe` False
    it "e6" $
      antisimetrica2 (R (fromList [1,2]) (fromList [(1,1),(2,1)])) `shouldBe` True
  describe "esTotal" $ do
    it "e1" $
      esTotal (R (fromList [1,3]) (fromList [(1,1),(3,1),(3,3)]))  `shouldBe`  True
    it "e2" $
      esTotal (R (fromList [1,3]) (fromList [(1,1),(3,1)])) `shouldBe`  False
    it "e3" $
      esTotal (R (fromList [1,3]) (fromList [(1,1),(3,3)])) `shouldBe`  False
  describe "clausuraReflexiva" $ do
    it "e1" $
      show (clausuraReflexiva (R (fromList [1,3]) (fromList [(1,1),(3,1)])))
      `shouldBe` "R (fromList [1,3]) (fromList [(1,1),(3,1),(3,3)])"
    it "e2" $
      property prop_ClausuraReflexiva
  describe "clausuraSimetrica" $ do
    it "e1" $
      show (clausuraSimetrica (R (fromList [1,3,5]) (fromList [(1,1),(3,1),(1,5)])))
      `shouldBe` "R (fromList [1,3,5]) (fromList [(1,1),(1,3),(1,5),(3,1),(5,1)])"
    it "e2" $
      property prop_ClausuraSimetrica
  describe "clausuraTransitiva" $ do
    it "e1" $
      show (clausuraTransitiva (R (fromList [1..6]) (fromList [(1,2),(2,5),(5,6)])))
      `shouldBe` "R (fromList [1,2,3,4,5,6]) (fromList [(1,2),(1,5),(1,6),(2,5),(2,6),(5,6)])"
    modifyMaxSize (const 7) $
      it "e2" $
        property prop_ClausuraTransitiva
