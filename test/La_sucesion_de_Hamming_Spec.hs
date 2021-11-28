module La_sucesion_de_Hamming_Spec (main, spec) where

import La_sucesion_de_Hamming
import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hamming" $
    it "e1" $
      take 12 hamming `shouldBe` [1,2,3,4,5,6,8,9,10,12,15,16]

  describe "divisoresPrimosEn1" $ do
    it "e1" $
      divisoresPrimosEn1 12 [2,3,5]  `shouldBe`  True
    it "e2" $
      divisoresPrimosEn1 14 [2,3,5]  `shouldBe`  False
    it "e3" $
      divisoresPrimosEn2 12 [2,3,5]  `shouldBe`  True
    it "e4" $
      divisoresPrimosEn2 14 [2,3,5]  `shouldBe`  False
    it "e5" $
      divisoresPrimosEn 12 [2,3,5]  `shouldBe`  True
    it "e6" $
      divisoresPrimosEn 14 [2,3,5]  `shouldBe`  False

  describe "hamming2" $
    it "e1" $
      take 12 hamming2  `shouldBe`  [1,2,3,4,5,6,8,9,10,12,15,16]

  describe "cantidadHammingMenores" $ do
    it "e1" $
      cantidadHammingMenores 6  `shouldBe`  5
    it "e2" $
      cantidadHammingMenores 7  `shouldBe`  6
    it "e3" $
      cantidadHammingMenores 8  `shouldBe`  6

  describe "siguienteHamming" $ do
    it "e1" $
      siguienteHamming 6   `shouldBe`  8
    it "e2" $
      siguienteHamming 21  `shouldBe`  24

  describe "huecoHamming" $ do
    it "e1" $
      take 4 (huecoHamming 2)   `shouldBe`  [(12,15),(20,24),(27,30),(32,36)]
    it "e2" $
      take 3 (huecoHamming 2)   `shouldBe`  [(12,15),(20,24),(27,30)]
    it "e3" $
      take 2 (huecoHamming 3)   `shouldBe`  [(20,24),(32,36)]
    it "e4" $
      head (huecoHamming 10)    `shouldBe`  (108,120)
    it "e5" $
      head (huecoHamming 1000)  `shouldBe`  (34992,36000)

  describe "prop_Hamming" $
    it "e1" $
      property prop_Hamming
