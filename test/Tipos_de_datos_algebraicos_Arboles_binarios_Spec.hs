module Tipos_de_datos_algebraicos_Arboles_binarios_Spec (main, spec) where

import Tipos_de_datos_algebraicos_Arboles_binarios
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "nHojas" $
    it "e1" $
      nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  `shouldBe`  3

  describe "nNodos" $ do
    it "e1" $
      nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  `shouldBe`  2
    it "p1" $
      property prop_nHojas

  describe "profundidad" $ do
    it "e1" $
      profundidad (N 9 (N 3 (H 2) (H 4)) (H 7))              `shouldBe`  2
    it "e2" $
      profundidad (N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7))  `shouldBe`  3
    it "e3" $
      profundidad (N 4 (N 5 (H 4) (H 2)) (N 3 (H 7) (H 4)))  `shouldBe`  2
    it "p1" $
      property prop_nNodosProfundidad

  describe "preorden" $ do
    it "e1" $
      preorden (N 9 (N 3 (H 2) (H 4)) (H 7))  `shouldBe`  [9,3,2,4,7]
    it "p1" $
      property prop_length_preorden

  describe "postorden" $
    it "e1" $
      postorden (N 9 (N 3 (H 2) (H 4)) (H 7))  `shouldBe`  [2,4,3,7,9]

  describe "preordenIt" $ do
    it "e1" $
      preordenIt (N 9 (N 3 (H 2) (H 4)) (H 7))  `shouldBe`  [9,3,2,4,7]
    it "p1" $
      property prop_preordenIt

  describe "espejo" $ do
    it "e1" $
      show (espejo (N 9 (N 3 (H 2) (H 4)) (H 7))) `shouldBe` "N 9 (H 7) (N 3 (H 4) (H 2))"
    it "p1" $
      property prop_espejo
    it "p2" $
      property prop_reverse_preorden_espejo
    it "p3" $
      property prop_recorrido

  describe "takeArbol" $ do
    it "e1" $
      show (takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7))) `shouldBe` "H 9"
    it "e2" $
      show (takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7))) `shouldBe` "N 9 (H 3) (H 7)"
    it "e3" $
      show (takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7))) `shouldBe` "N 9 (N 3 (H 2) (H 4)) (H 7)"
    it "e4" $
      show (takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7))) `shouldBe` "N 9 (N 3 (H 2) (H 4)) (H 7)"
    it "p1" $
      property prop_takeArbol

  describe "repeatArbol" $ do
    it "e1" $
      show (takeArbol 0 (repeatArbol 3)) `shouldBe` "H 3"
    it "e2" $
      show (takeArbol 1 (repeatArbol 3)) `shouldBe` "N 3 (H 3) (H 3)"
    it "e3" $
      show (takeArbol 2 (repeatArbol 3)) `shouldBe` "N 3 (N 3 (H 3) (H 3)) (N 3 (H 3) (H 3))"

  describe "replicateArbol" $ do
    it "e1" $
      show (replicateArbol 0 5) `shouldBe` "H 5"
    it "e2" $
      show (replicateArbol 1 5) `shouldBe` "N 5 (H 5) (H 5)"
    it "e3" $
      show (replicateArbol 2 5) `shouldBe` "N 5 (N 5 (H 5) (H 5)) (N 5 (H 5) (H 5))"
    modifyMaxSize (const 7) $
      it "p1" $
      property prop_replicateArbol

  describe "" $ do
    it "e1" $
      show (mapArbol (*2) (N 9 (N 3 (H 2) (H 4)) (H 7))) `shouldBe`
        "N 18 (N 6 (H 4) (H 8)) (H 14)"
    it "p1" $
      property prop_mapArbol_espejo
    it "p2" $
      property prop_map_preorden
