module Programación_dinamica_Caminos_en_una_reticula_Spec (main, spec) where

import Programación_dinamica_Caminos_en_una_reticula
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "caminosR" $ do
    it "e1" $
      caminosR (2,3) `shouldBe`
      [[(1,1),(1,2),(1,3),(2,3)],
       [(1,1),(1,2),(2,2),(2,3)],
       [(1,1),(2,1),(2,2),(2,3)]]
    it "e2" $
      caminosR (3,4) `shouldBe`
      [[(1,1),(1,2),(1,3),(1,4),(2,4),(3,4)],
       [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4)],
       [(1,1),(1,2),(2,2),(2,3),(2,4),(3,4)],
       [(1,1),(2,1),(2,2),(2,3),(2,4),(3,4)],
       [(1,1),(1,2),(1,3),(2,3),(3,3),(3,4)],
       [(1,1),(1,2),(2,2),(2,3),(3,3),(3,4)],
       [(1,1),(2,1),(2,2),(2,3),(3,3),(3,4)],
       [(1,1),(1,2),(2,2),(3,2),(3,3),(3,4)],
       [(1,1),(2,1),(2,2),(3,2),(3,3),(3,4)],
       [(1,1),(2,1),(3,1),(3,2),(3,3),(3,4)]]

  describe "caminosPD" $ do
    it "e1" $
      caminosPD (2,3) `shouldBe`
      [[(1,1),(1,2),(1,3),(2,3)],
       [(1,1),(1,2),(2,2),(2,3)],
       [(1,1),(2,1),(2,2),(2,3)]]
    it "e2" $
      caminosPD (3,4) `shouldBe`
      [[(1,1),(1,2),(1,3),(1,4),(2,4),(3,4)],
       [(1,1),(1,2),(1,3),(2,3),(2,4),(3,4)],
       [(1,1),(1,2),(2,2),(2,3),(2,4),(3,4)],
       [(1,1),(2,1),(2,2),(2,3),(2,4),(3,4)],
       [(1,1),(1,2),(1,3),(2,3),(3,3),(3,4)],
       [(1,1),(1,2),(2,2),(2,3),(3,3),(3,4)],
       [(1,1),(2,1),(2,2),(2,3),(3,3),(3,4)],
       [(1,1),(1,2),(2,2),(3,2),(3,3),(3,4)],
       [(1,1),(2,1),(2,2),(3,2),(3,3),(3,4)],
       [(1,1),(2,1),(3,1),(3,2),(3,3),(3,4)]]

  describe "nCaminosCR" $ do
    it "e1" $
      nCaminosCR (2,3) `shouldBe`  3
    it "e2" $
      nCaminosCR (3,4) `shouldBe`  10

  describe "nCaminosCPD" $ do
    it "e1" $
      nCaminosCPD (2,3) `shouldBe`  3
    it "e2" $
      nCaminosCPD (3,4) `shouldBe`  10

  describe "nCaminosR" $ do
    it "e1" $
      nCaminosR (2,3) `shouldBe`  3
    it "e2" $
      nCaminosR (3,4) `shouldBe`  10

  describe "nCaminosPD" $
    it "e1" $
      nCaminosPD (3,4) `shouldBe`  10

  describe "nCaminosF" $
    it "e1" $
      nCaminosF (8,8) `shouldBe`  3432

  describe "nCaminosFS" $ do
    it "e1" $
      nCaminosFS (8,8) `shouldBe`  3432
