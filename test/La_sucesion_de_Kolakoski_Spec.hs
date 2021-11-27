module La_sucesion_de_Kolakoski_Spec (main, spec) where

import La_sucesion_de_Kolakoski
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "contadora" $ do
    it "e1" $
      contadora "abbaaabbb"        `shouldBe`  [1,2,3,3]
    it "e2" $
      contadora "122112122121121"  `shouldBe`  [1,2,2,1,1,2,1,1,2,1,1]
    it "e3" $
      contadora2 "abbaaabbb"        `shouldBe`  [1,2,3,3]
    it "e4" $
      contadora2 "122112122121121"  `shouldBe`  [1,2,2,1,1,2,1,1,2,1,1]
    it "e5" $
      contadora3 "abbaaabbb"        `shouldBe`  [1,2,3,3]
    it "e6" $
      contadora3 "122112122121121"  `shouldBe`  [1,2,2,1,1,2,1,1,2,1,1]

  describe "contada" $ do
    it "e1" $
      contada [1,2,3,3] "ab"                `shouldBe`  "abbaaabbb"
    it "e2" $
      contada [1,2,3,3] "abc"               `shouldBe`  "abbcccaaa"
    it "e3" $
      contada [1,2,2,1,1,2,1,1,2,1,1] "12"  `shouldBe`  "122112122121121"

  describe "autocontadora" $ do
    it "e1" $
      take 11 autocontadora  `shouldBe`  [1,2,2,1,1,2,1,2,2,1,2]
    it "e2" $
      take 12 autocontadora  `shouldBe`  [1,2,2,1,1,2,1,2,2,1,2,2]
    it "e3" $
      take 18 autocontadora  `shouldBe`  [1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2]
    it "e4" $
      take 11 autocontadora2  `shouldBe`  [1,2,2,1,1,2,1,2,2,1,2]
    it "e5" $
      take 12 autocontadora2  `shouldBe`  [1,2,2,1,1,2,1,2,2,1,2,2]
    it "e6" $
      take 18 autocontadora2  `shouldBe`  [1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2]
