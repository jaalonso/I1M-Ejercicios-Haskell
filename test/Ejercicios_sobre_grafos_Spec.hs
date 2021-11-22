module Ejercicios_sobre_grafos_Spec (main, spec) where

import Ejercicios_sobre_grafos
import I1M.Grafo
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "recorridos" $
    it "e1" $
      recorridos [2,5,3] `shouldBe`
      [[2,5,3,2],[5,2,3,5],[3,5,2,3],[5,3,2,5],[3,2,5,3],[2,3,5,2]]

  describe "anchura" $ do
    it "e1" $
      anchura grafo2  `shouldBe`  4
    it "e2" $
      anchura2 grafo2  `shouldBe`  4

  describe "conexo" $ do
    it "e1" $
      conexo (creaGrafo ND (1,3) [(1,2,0),(3,2,0)])          `shouldBe`  True
    it "e2" $
      conexo (creaGrafo ND (1,4) [(1,2,0),(3,2,0),(4,1,0)])  `shouldBe`  True
    it "e3" $
      conexo (creaGrafo ND (1,4) [(1,2,0),(3,4,0)])          `shouldBe`  False

  describe "correcta" $ do
    it "e1" $
      correcta [(1,A),(2,B),(3,B),(4,C),(5,A),(6,A),(7,B)] mapa `shouldBe` True
    it "e2" $
      correcta [(1,A),(2,B),(3,A),(4,C),(5,A),(6,A),(7,B)] mapa `shouldBe` False

  describe "aislados" $
    it "e1" $
      aislados grafo5 `shouldBe` [1,2,4]

  describe "conectados" $ do
    it "e1" $
      conectados grafo6 1 3  `shouldBe`  True
    it "e2" $
      conectados grafo6 1 4  `shouldBe`  False
    it "e3" $
      conectados grafo6 6 2  `shouldBe`  False
    it "e4" $
      conectados grafo6 3 1  `shouldBe`  True
