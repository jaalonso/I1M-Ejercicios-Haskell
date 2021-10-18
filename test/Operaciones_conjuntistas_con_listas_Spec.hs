module Operaciones_conjuntistas_con_listas_Spec (main, spec) where

import Operaciones_conjuntistas_con_listas
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSize)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "subconjunto" $ do
    it "e1" $
      subconjunto [3::Int,2,3] [2,5,3,5]  `shouldBe`  True
    it "e2" $
      subconjunto [3::Int,2,3] [2,5,6,5]  `shouldBe`  False
    it "e3" $
      subconjuntoR [3::Int,2,3] [2,5,3,5]  `shouldBe`  True
    it "e4" $
      subconjuntoR [3::Int,2,3] [2,5,6,5]  `shouldBe`  False
    it "e5" $
      property prop_subconjuntoR
    it "e6" $
      subconjuntoA [3::Int,2,3] [2,5,3,5]  `shouldBe`  True
    it "e7" $
      subconjuntoA [3::Int,2,3] [2,5,6,5]  `shouldBe`  False
    it "e8" $
      property prop_subconjuntoA

  describe "iguales" $ do
    it "e1" $
      iguales [3::Int,2,3] [2,3]    `shouldBe`  True
    it "e2" $
      iguales [3::Int,2,3] [2,3,2]  `shouldBe`  True
    it "e3" $
      iguales [3::Int,2,3] [2,3,4]  `shouldBe`  False
    it "e4" $
      iguales [2::Int,3] [4,5]      `shouldBe`  False

  describe "union" $ do
    it "e1" $
      union [3::Int,2,5] [5,7,3,4]  `shouldMatchList`  [3,2,5,7,4]
    it "e2" $
      unionR [3::Int,2,5] [5,7,3,4]  `shouldMatchList`  [3,2,5,7,4]
    it "e3" $
      property prop_union

  describe "prop_union_conmutativa" $
    it "p1" $
      property prop_union_conmutativa

  describe "interseccion" $ do
    it "e1" $
      interseccion [3::Int,2,5] [5,7,3,4]  `shouldMatchList`  [3,5]
    it "e2" $
      interseccion [3::Int,2,5] [9,7,6,4]  `shouldBe`  []
    it "e3" $
      interseccionR [3::Int,2,5] [5,7,3,4]  `shouldMatchList`  [3,5]
    it "e4" $
      interseccionR [3::Int,2,5] [9,7,6,4]  `shouldBe`  []
    it "e5" $
      property prop_interseccion

  describe "producto" $ do
    it "e1" $
      producto [1::Int,3] [2::Int,4] `shouldBe` [(1,2),(1,4),(3,2),(3,4)]
    it "e2" $
      productoR [1::Int,3] [2::Int,4] `shouldBe` [(1,2),(1,4),(3,2),(3,4)]
    it "e3" $
      property prop_producto

  describe "prop_elementos_producto" $
    it "p1" $
      property prop_elementos_producto

  describe "subconjuntos" $ do
    it "e1" $
      subconjuntos [2::Int,3,4]
      `shouldMatchList` [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
    it "e1" $
      subconjuntos [1::Int,2,3,4]
      `shouldMatchList` [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],
                         [1],[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
    it "e3" $
      subconjuntos' [2::Int,3,4]
      `shouldMatchList` [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
    it "e4" $
      subconjuntos' [1::Int,2,3,4]
      `shouldMatchList` [[1,2,3,4],[1,2,3],[1,2,4],[1,2],[1,3,4],[1,3],[1,4],
                         [1],[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]

  describe "prop_subconjuntos" $
    modifyMaxSize (const 7) $
      it "p1" $
      property prop_subconjuntos
