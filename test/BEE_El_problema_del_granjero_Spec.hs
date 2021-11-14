module BEE_El_problema_del_granjero_Spec (main, spec) where

import BEE_El_problema_del_granjero
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "seguro" $ do
    it "e1" $
      seguro (I,D,D,I)  `shouldBe`  False
    it "e2" $
      seguro (D,D,D,I)  `shouldBe`  True
    it "e3" $
      seguro (D,D,I,I)  `shouldBe`  False
    it "e4" $
      seguro (I,D,I,I)  `shouldBe`  True
    it "e5" $
      seguro2 (I,D,D,I)  `shouldBe`  False
    it "e6" $
      seguro2 (D,D,D,I)  `shouldBe`  True
    it "e7" $
      seguro2 (D,D,I,I)  `shouldBe`  False
    it "e8" $
      seguro2 (I,D,I,I)  `shouldBe`  True

  describe "sucesoresE" $ do
    it "e1" $
      sucesoresE (I,I,I,I)  `shouldBe`  [(D,I,D,I)]
    it "e2" $
      sucesoresE (D,I,D,I)  `shouldBe`  [(I,I,D,I),(I,I,I,I)]

  describe "sucesoresN" $
    it "e1" $
      sucesoresN (Nodo [(I,I,D,I),(D,I,D,I),(I,I,I,I)]) `shouldBe`
      [Nodo [(D,D,D,I),(I,I,D,I),(D,I,D,I),(I,I,I,I)],
       Nodo [(D,I,D,D),(I,I,D,I),(D,I,D,I),(I,I,I,I)]]

  describe "esFinal" $ do
    it "e1" $
      esFinal (Nodo [(D,D,D,D),(I,I,I,I)])  `shouldBe`  True
    it "e2" $
      esFinal (Nodo [(I,I,D,I),(I,I,I,I)])  `shouldBe`  False

  describe "granjeroEE" $
    it "e1" $
      head granjeroEE `shouldBe`
      Nodo [(D,D,D,D),(I,D,I,D),(D,D,I,D),(I,D,I,I),
            (D,D,D,I),(I,I,D,I),(D,I,D,I),(I,I,I,I)]
