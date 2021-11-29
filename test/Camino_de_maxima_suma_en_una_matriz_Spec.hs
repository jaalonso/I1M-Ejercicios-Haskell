module Camino_de_maxima_suma_en_una_matriz_Spec (main, spec) where

import Camino_de_maxima_suma_en_una_matriz
import Data.Matrix
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "caminos" $ do
    it "e1" $
      caminos1 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldMatchList`
      [[1,7, 3,8,4,9],
       [1,7,12,8,4,9],
       [1,7,12,3,4,9],
       [1,7,12,3,8,9],
       [1,6,12,8,4,9],
       [1,6,12,3,4,9],
       [1,6,12,3,8,9],
       [1,6,11,3,4,9],
       [1,6,11,3,8,9],
       [1,6,11,2,8,9]]
    it "e2" $
      caminos2 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldMatchList`
      [[1,7, 3,8,4,9],
       [1,7,12,8,4,9],
       [1,7,12,3,4,9],
       [1,7,12,3,8,9],
       [1,6,12,8,4,9],
       [1,6,12,3,4,9],
       [1,6,12,3,8,9],
       [1,6,11,3,4,9],
       [1,6,11,3,8,9],
       [1,6,11,2,8,9]]
    it "e3" $
      caminos3 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldMatchList`
      [[1,7, 3,8,4,9],
       [1,7,12,8,4,9],
       [1,7,12,3,4,9],
       [1,7,12,3,8,9],
       [1,6,12,8,4,9],
       [1,6,12,3,4,9],
       [1,6,12,3,8,9],
       [1,6,11,3,4,9],
       [1,6,11,3,8,9],
       [1,6,11,2,8,9]]

  describe "maximaSuma" $ do
    it "e1" $
      maximaSuma1 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
    it "e2" $
      maximaSuma1a (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
    it "e3" $
      maximaSuma2 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
    it "e4" $
      maximaSuma2a (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
    it "e5" $
      maximaSuma3 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
    it "e6" $
      maximaSuma3a (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
    it "e7" $
      maximaSuma4 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41
    it "e8" $
      maximaSuma5 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]]) `shouldBe` 41

  describe "caminoMaxSuma" $ do
    it "e1" $
      caminoMaxSuma1 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
      `shouldBe` [1,7,12,8,4,9]
    it "e2" $
      caminoMaxSuma2 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
      `shouldBe` [1,7,12,8,4,9]
    it "e3" $
      caminoMaxSuma3 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
      `shouldBe` [1,7,12,8,4,9]
    it "e4" $
      caminoMaxSuma4 (fromLists [[1,6,11,2],[7,12,3,8],[3,8,4,9]])
      `shouldBe` [1,7,12,8,4,9]
