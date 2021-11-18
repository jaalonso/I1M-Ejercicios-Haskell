module Estadistica_descriptiva_con_librerias_Spec (main, spec) where

import Estadistica_descriptiva_con_librerias
import Test.Hspec

infix 5 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < 0.001

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "media" $
    it "e1" $
      media [4,8,4,5,9]  `shouldBe`  6.0

  describe "mediaGeometrica" $ do
    it "e1" $
      mediaGeometrica [2,18]   `shouldBe`  6.0
    it "e2" $
      mediaGeometrica [3,1,9]  `shouldBe`  3.0000000000000004

  describe "rango" $
    it "e1" $
      rango [4,2,4,7,3]  `shouldBe`  5.0

  describe "varianza" $ do
    it "e1" $
      varianza [4,8,4,5,9]       `shouldBe`  4.4
    it "e2" $
      varianza (replicate 10 3)  `shouldBe`  0.0

  describe "desviacionTipica" $ do
    it "e1" $
      desviacionTipica [4,8,4,5,9]       `shouldBe`  2.0976176963403033
    it "e2" $
      desviacionTipica (replicate 10 3)  `shouldBe`  0.0

  describe "regresionLineal" $
    it "e1" $
      let (a,b) = regresionLineal ejX ejY
      in (a ~= 5.195045748716805) && (b ~= 0.9218924347243919)
      `shouldBe` True
