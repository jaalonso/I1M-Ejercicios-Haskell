module Programacion_dinamica_Turista_en_Manhattan_Spec where

import Programacion_dinamica_Turista_en_Manhattan
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mayorNumeroVR" $ do
    it "e1" $
      mayorNumeroVR ej1 `shouldBe` 34
    it "e2" $
      mayorNumeroVR ej2 `shouldBe` 4
    it "e3" $
      mayorNumeroVR ej3 `shouldBe` 17

  describe "mayorNumeroVPD" $ do
    it "e1" $
      mayorNumeroVPD ej1 `shouldBe` 34
    it "e2" $
      mayorNumeroVPD ej2 `shouldBe` 4
    it "e3" $
      mayorNumeroVPD ej3 `shouldBe` 17
