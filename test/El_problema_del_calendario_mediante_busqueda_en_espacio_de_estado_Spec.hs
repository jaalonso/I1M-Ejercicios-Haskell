module El_problema_del_calendario_mediante_busqueda_en_espacio_de_estado_Spec (main, spec) where

import El_problema_del_calendario_mediante_busqueda_en_espacio_de_estado
import Data.Matrix
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inicial" $
    it "e1" $
      toLists (inicial 4)
      `shouldBe` [[0,0,0],[0,0,0],[0,0,0],[0,0,0]]

  describe "sucesores" $ do
    it "e1" $
      map toLists (sucesores 4 (fromLists [[2,3,0],[1,0,0],[0,1,0],[0,0,0]]))
      `shouldBe` [[[2,3,4],[1,0,0],[0,1,0],[0,0,1]]]
    it "e2" $
      map toLists (sucesores 4 (fromLists [[2,3,4],[1,0,0],[0,1,0],[0,0,1]]))
      `shouldBe` [[[2,3,4],[1,4,0],[0,1,0],[0,2,1]]]

  describe "esFinal" $ do
    it "e1" $
      esFinal 4 (fromLists [[2,3,4],[1,4,3],[4,1,2],[3,2,1]])
      `shouldBe` True
    it "e2" $
      esFinal 4 (fromLists [[2,3,4],[1,4,3],[4,1,2],[3,2,0]])
      `shouldBe` False

  describe "calendario" $
    it "e1" $
      toLists (head (calendario 6))
      `shouldBe` [[2,3,4,5,6],[1,4,5,6,3],[5,1,6,4,2],[6,2,1,3,5],[3,6,2,1,4],[4,5,3,2,1]]
