module Resolucion_de_problemas_mediante_busqueda_en_espacios_de_estados_Spec (main, spec) where

import Resolucion_de_problemas_mediante_busqueda_en_espacios_de_estados
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "domino" $ do
    it "e1" $
      domino [(1,2),(2,3),(1,4)]
      `shouldBe` [[(4,1),(1,2),(2,3)],[(3,2),(2,1),(1,4)]]
    it "e2" $
      domino [(1,2),(1,1),(1,4)]
      `shouldBe` [[(4,1),(1,1),(1,2)],[(2,1),(1,1),(1,4)]]
    it "e3" $
      domino [(1,2),(3,4),(2,3)]
      `shouldBe` [[(1,2),(2,3),(3,4)],[(4,3),(3,2),(2,1)]]
    it "e4" $
      domino [(1,2),(2,3),(5,4)]
      `shouldBe` []

  describe "suma0" $ do
    it "e1" $
      suma0 [-7,-3,-2,5,8]
      `shouldBe` [[-3,-2,5]]
    it "e2" $
      suma0 [-7,-3,-2,5,8,-1]
      `shouldBe` [[-7,-3,-2,-1,5,8],[-7,-1,8],[-3,-2,5]]
    it "e3" $
      suma0 [-7,-3,1,5,8]
      `shouldBe` []

  describe "jarras" $
    it "e1" $
      jarras !! 4
      `shouldBe` [(0,0),(4,0),(1,3),(1,0),(0,1),(4,1),(2,3)]
