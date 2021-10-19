module El_algoritmo_de_Luhn_Spec (main, spec) where

import El_algoritmo_de_Luhn
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "digitosInv" $
    it "e1" $
      digitosInv 320274  `shouldBe`  [4,7,2,0,2,3]

  describe "digitosInv2" $
    it "e1" $
      digitosInv2 320274  `shouldBe`  [4,7,2,0,2,3]

  describe "doblePosImpar" $ do
    it "e1" $
      doblePosImpar [4,9,5,5]    `shouldBe`  [4,18,5,10]
    it "e2" $
      doblePosImpar [4,9,5,5,7]  `shouldBe`  [4,18,5,10,7]
    prop "p1" $
      \xs -> all (== doblePosImpar xs)
                 [doblePosImpar2 xs,
                  doblePosImpar3 xs]

  describe "sumaDigitos" $ do
    it "e1" $
      sumaDigitos [10,5,18,4] `shouldBe` 19
    prop "p1" $
      \xs -> all (== sumaDigitos xs)
                 [sumaDigitos2 xs,
                  sumaDigitos3 xs,
                  sumaDigitos4 xs]

  describe "ultimoDigito" $ do
    it "e1" $
      ultimoDigito 123 `shouldBe` 3
    it "e2" $
      ultimoDigito   0 `shouldBe` 0

  describe "luhn" $ do
    it "e1" $
      luhn 5594589764218858  `shouldBe`  True
    it "e1" $
      luhn 1234567898765432  `shouldBe`  False
    prop "p1" $
      \n -> luhn n == luhn2 n
