module Mayorias_parlamentarias_Spec (main, spec) where

import Mayorias_parlamentarias
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "partidos" $ do
    it "e1" $
      partidos [(P1,3),(P3,5),(P4,3)]  `shouldBe`  [P1,P3,P4]
    it "e2" $
      partidos2 [(P1,3),(P3,5),(P4,3)]  `shouldBe`  [P1,P3,P4]

  describe "parlamentarios" $ do
    it "e1" $
      parlamentarios [(P1,3),(P3,5),(P4,3)]  `shouldBe`  11
    it "e2" $
      parlamentarios2 [(P1,3),(P3,5),(P4,3)]  `shouldBe`  11

  describe "busca" $ do
    it "e1" $
      busca P3 [(P1,2),(P3,19)]
      `shouldBe` 19
    it "e2" $
      busca2 P3 [(P1,2),(P3,19)]
      `shouldBe` 19

  describe "busca'" $ do
    it "e1" $
      busca' P3 [(P1,2),(P3,19)]   `shouldBe`   Just 19
    it "e2" $
      busca' P8 [(P1,2),(P3,19)]   `shouldBe`   Nothing
    it "e3" $
      busca'2 P3 [(P1,2),(P3,19)]   `shouldBe`   Just 19
    it "e4" $
      busca'2 P8 [(P1,2),(P3,19)]   `shouldBe`   Nothing

  describe "prop_BuscaNothing" $
    it "e1" $
      property prop_BuscaNothing

  describe "prop_BuscaEquivLookup" $
    it "e1" $
      property prop_BuscaEquivLookup

  describe "mayoria" $ do
    it "e1" $
      mayoria [(P1,3),(P3,5),(P4,3)]   `shouldBe`   6
    it "e2" $
      mayoria [(P1,3),(P3,6)]          `shouldBe`   5

  describe "coaliciones" $ do
    it "e1" $
      coaliciones [(P1,3),(P2,2),(P3,1)] 3   `shouldBe`  [[P2,P3],[P1]]
    it "e2" $
      coaliciones [(P1,3),(P3,5),(P4,3)] 6   `shouldBe`  [[P3,P4],[P1,P4],[P1,P3]]
    it "e3" $
      coaliciones [(P1,3),(P3,5),(P4,3)] 9   `shouldBe`  [[P1,P3,P4]]
    it "e4" $
      coaliciones [(P1,3),(P3,5),(P4,3)] 14  `shouldBe`  []
    it "e5" $
      coaliciones [(P1,3),(P3,5),(P4,3)] 2   `shouldBe`  [[P4],[P3],[P1]]
    it "e6" $
      coaliciones [(P1,2),(P3,5),(P4,3)] 6   `shouldBe`  [[P3,P4],[P1,P3]]

  describe "mayorias" $ do
    it "e1" $
      mayorias [(P1,3),(P3,5),(P4,3)]   `shouldBe`   [[P3,P4],[P1,P4],[P1,P3]]
    it "e2" $
      mayorias [(P1,2),(P3,5),(P4,3)]   `shouldBe`   [[P3,P4],[P1,P3]]

  describe "esMayoritaria" $ do
    it "e1" $
      esMayoritaria [P3,P4] [(P1,3),(P3,5),(P4,3)]   `shouldBe`   True
    it "e2" $
      esMayoritaria [P4] [(P1,3),(P3,5),(P4,3)]      `shouldBe`   False

  describe "prop_MayoriasSonMayoritarias" $
    it "e1" $
      property prop_MayoriasSonMayoritarias

  describe "esMayoritariaMinimal" $ do
    it "e1" $
      esMayoritariaMinimal [P3,P4] [(P1,3),(P3,5),(P4,3)]     `shouldBe`  True
    it "e2" $
      esMayoritariaMinimal [P1,P3,P4] [(P1,3),(P3,5),(P4,3)]  `shouldBe`  False

  describe "coalicionesMinimales" $ do
    it "e1" $
      coalicionesMinimales [(P1,3),(P3,5),(P4,3)] 6
      `shouldBe` [([P3,P4],8),([P1,P4],6),([P1,P3],8)]
    it "e2" $
      coalicionesMinimales [(P1,3),(P3,5),(P4,3)] 5
      `shouldBe` [([P3],5),([P1,P4],6)]

  describe "mayoriasMinimales" $
    it "e1" $
      mayoriasMinimales [(P1,3),(P3,5),(P4,3)] `shouldBe` [[P3,P4],[P1,P4],[P1,P3]]

  describe "prop_MayoriasMinimalesSonMayoritariasMinimales" $
    it "e1" $
      property prop_MayoriasMinimalesSonMayoritariasMinimales
