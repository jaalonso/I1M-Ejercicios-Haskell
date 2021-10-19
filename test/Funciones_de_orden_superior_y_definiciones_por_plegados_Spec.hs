module Funciones_de_orden_superior_y_definiciones_por_plegados_Spec (main, spec) where

import Funciones_de_orden_superior_y_definiciones_por_plegados
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "segmentos" $ do
    it "e1" $
      segmentos even [1,2,0,4,9,6,4,5,7,2]  `shouldBe`  [[2,0,4],[6,4],[2]]
    it "e2" $
      segmentos odd  [1,2,0,4,9,6,4,5,7,2]  `shouldBe`  [[1],[9],[5,7]]

  describe "relacionadosC" $ do
    it "e1" $
      relacionadosC (<) [2,3,7,9]                `shouldBe`  True
    it "e2" $
      relacionadosC (<) [2,3,1,9]                `shouldBe`  False

  describe "relacionadosR" $ do
    it "e1" $
      relacionadosR (<) [2,3,7,9]                `shouldBe`  True
    it "e2" $
      relacionadosR (<) [2,3,1,9]                `shouldBe`  False

  describe "agrupa" $ do
    it "e1" $
      agrupa [[1..6],[7..9],[10..20]]  `shouldBe`  [[1,7,10],[2,8,11],[3,9,12]]
    it "e2" $
      agrupa ([]::[[Int]])             `shouldBe`  ([]::[[Int]])

  describe "prop_agrupa" $
    it "p1" $
      property prop_agrupa

  describe "concatR" $
    it "e1" $
      concatR [[1,3],[2,4,6],[1,9]]  `shouldBe`  [1,3,2,4,6,1,9]

  describe "concatP" $
    it "e1" $
      concatP [[1,3],[2,4,6],[1,9]]  `shouldBe`  [1,3,2,4,6,1,9]

  describe "prop_concat" $
    it "p1" $
      property prop_concat

  describe "prop_longConcat" $
    it "p1" $
      property prop_longConcat

  describe "filtraAplicaC" $
    it "e1" $
      filtraAplicaC (4+) (<3) [1..7]  `shouldBe`  [5,6]

  describe "filtraAplicaMF" $
    it "e1" $
      filtraAplicaMF (4+) (<3) [1..7]  `shouldBe`  [5,6]

  describe "filtraAplicaR" $
    it "e1" $
      filtraAplicaR (4+) (<3) [1..7]  `shouldBe`  [5,6]

  describe "filtraAplicaP" $ do
    it "e1" $
      filtraAplicaP (4+) (<3) [1..7]  `shouldBe`  [5,6]
    it "e2" $
      filtraAplicaP2 (4+) (<3) [1..7]  `shouldBe`  [5,6]

  describe "maximumR" $ do
    it "e1" $
      maximumR [3,7,2,5]                  `shouldBe`  7
    it "e2" $
      maximumR ["todo","es","falso"]      `shouldBe`  "todo"
    it "e3" $
      maximumR ["menos","alguna","cosa"]  `shouldBe`  "menos"

  describe "maximumP" $ do
    it "e1" $
      maximumP [3,7,2,5]                  `shouldBe`  7
    it "e2" $
      maximumP ["todo","es","falso"]      `shouldBe`  "todo"
    it "e3" $
      maximumP ["menos","alguna","cosa"]  `shouldBe`  "menos"
