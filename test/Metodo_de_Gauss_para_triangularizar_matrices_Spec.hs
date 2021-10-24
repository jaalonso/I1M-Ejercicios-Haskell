{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Metodo_de_Gauss_para_triangularizar_matrices_Spec (main, spec) where

import Metodo_de_Gauss_para_triangularizar_matrices
import Data.Array
import Data.Ratio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "listaMatriz" $
    it "e1" $
      listaMatriz [[1,3,5],[2,4,7]]
        `shouldBe` array ((1,1),(2,3)) [((1,1),1),((1,2),3),((1,3),5),
                          ((2,1),2),((2,2),4),((2,3),7)]

  describe "separa" $
    it "e1" $
      separa 3 [1..11]  `shouldBe`  [[1,2,3],[4,5,6],[7,8,9],[10,11]]

  describe "matrizLista" $
    it "e1" $
      matrizLista (listaMatriz [[5,1,0],[3,2,6]])
        `shouldBe` [[5,1,0],[3,2,6]]

  describe "numFilas" $
    it "e1" $
      numFilas (listaMatriz [[1,3,5],[2,4,7]])  `shouldBe`  2

  describe "numColumnas" $
    it "e1" $
      numColumnas (listaMatriz [[1,3,5],[2,4,7]])  `shouldBe`  3

  describe "dimension" $
    it "e1" $
      dimension (listaMatriz [[1,3,5],[2,4,7]])  `shouldBe`  (2,3)

  describe "diagonalPral" $ do
    let p = listaMatriz [[5,1,0],[3,2,6]]
    it "e1" $
      diagonalPral p `shouldBe` array (1,2) [(1,5),(2,2)]
    it "e2" $
      elems (diagonalPral p) `shouldBe` [5,2]

  describe "intercambiaFilas" $ do
    let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
    it "e1" $
      intercambiaFilas 1 3 p `shouldBe`
      array ((1,1),(3,3)) [((1,1),4),((1,2),6),((1,3),9),
                           ((2,1),3),((2,2),2),((2,3),6),
                           ((3,1),5),((3,2),1),((3,3),0)]
    it "e2" $
      matrizLista (intercambiaFilas 1 3 p) `shouldBe`
      [[4,6,9],[3,2,6],[5,1,0]]

  describe "intercambiaColumnas" $
    it "e1" $
      matrizLista (intercambiaColumnas 1 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]]))
      `shouldBe` [[0,1,5],[6,2,3],[9,6,4]]

  describe "multFilaPor" $
    it "e1" $
      matrizLista (multFilaPor 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]]))
      `shouldBe` [[5,1,0],[9,6,18],[4,6,9]]

  describe "sumaFilaFila" $
    it "e1" $
      matrizLista (sumaFilaFila 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]]))
      `shouldBe` [[5,1,0],[7,8,15],[4,6,9]]

  describe "sumaFilaPor" $
    it "e1" $
      matrizLista (sumaFilaPor 2 3 10 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]]))
      `shouldBe` [[5,1,0],[43,62,96],[4,6,9]]

  describe "buscaIndiceDesde" $ do
    it "e1" $
      buscaIndiceDesde (listaMatriz [[5,1,0],[3,2,6],[4,6,9]]) 3 2
      `shouldBe` Just 2
    it "e2" $
      buscaIndiceDesde (listaMatriz [[5,1,1],[3,2,0],[4,6,0]]) 3 2
      `shouldBe` Nothing

  describe "buscaPivoteDesde" $ do
    it "e1" $
      buscaPivoteDesde (listaMatriz [[5,1,0],[3,2,6],[4,6,9]]) 3 2
      `shouldBe` Just 6
    it "e2" $
      buscaPivoteDesde (listaMatriz [[5,1,1],[3,2,0],[4,6,0]]) 3 2
      `shouldBe` Nothing

  describe "anuladaColumnaDesde" $ do
    it "e1" $
      anuladaColumnaDesde (listaMatriz [[5,1,1],[3,2,0],[4,6,0]]) 3 2
      `shouldBe` True
    it "e2" $
      anuladaColumnaDesde (listaMatriz [[5,1,0],[3,2,6],[4,6,9]]) 3 2
      `shouldBe` False

  describe "anulaEltoColumnaDesde" $
    it "e1" $
      matrizLista (anulaEltoColumnaDesde (listaMatriz [[2,3,1],[5,0,5],[8,6,9]] :: Matriz Double) 2 1)
      `shouldBe` [[2.0,3.0,1.0],[5.0,0.0,5.0],[4.0,0.0,7.0]]

  describe "anulaColumnaDesde" $ do
    let p1 = listaMatriz [[2,2,1],[5,4,5],[10,8,9]] :: Matriz Double
        p2 = listaMatriz [[4,5],[2,7%2],[6,10]]
    it "e1" $
      matrizLista (anulaColumnaDesde p1 2 1)
      `shouldBe` [[2.0,2.0,1.0],[1.0,0.0,3.0],[2.0,0.0,5.0]]
    it "e2" $
      matrizLista (anulaColumnaDesde p2 1 1)
      `shouldBe` [[4 % 1,5 % 1],[0 % 1,1 % 1],[0 % 1,5 % 2]]

  describe "elementosNoNulosColDesde" $
    it "e1" $
      elementosNoNulosColDesde (listaMatriz [[3,2],[5,1],[0,4]]) 1 2
      `shouldBe` [5]

  describe "existeColNoNulaDesde" $ do
    it "e1" $
      existeColNoNulaDesde (listaMatriz [[3,2,5],[5,0,0],[6,0,0]]) 2 2
      `shouldBe` False
    it "e2" $
      existeColNoNulaDesde (listaMatriz [[3,2,5],[5,7,0],[6,0,0]]) 2 2
      `shouldBe` True

  describe "menorIndiceColNoNulaDesde" $ do
    it "e1" $
      menorIndiceColNoNulaDesde (listaMatriz [[3,2,5],[5,7,0],[6,0,0]]) 2 2
      `shouldBe` Just 2
    it "e2" $
      menorIndiceColNoNulaDesde (listaMatriz [[3,2,5],[5,0,0],[6,0,2]]) 2 2
      `shouldBe` Just 3
    it "e3" $
      menorIndiceColNoNulaDesde (listaMatriz [[3,2,5],[5,0,0],[6,0,0]]) 2 2
      `shouldBe` Nothing

  describe "menorIndiceColNoNulaDesde" $ do
    it "e1" $
      matrizLista (gaussAux (listaMatriz [[1.0,2,3],[1,2,4],[3,2,5]]) 2 2)
      `shouldBe` [[1.0,2.0,3.0],[1.0,2.0,4.0],[2.0,0.0,1.0]]

  describe "gauss" $ do
    let p = listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]]
    it "e1" $
      gauss p
      `shouldBe` array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),2.0),
                                      ((2,1),0.0),((2,2),1.0),((2,3),0.0),
                                      ((3,1),0.0),((3,2),0.0),((3,3),0.0)]
    it "e2" $
      matrizLista (gauss p)
      `shouldBe` [[1.0,3.0,2.0],[0.0,1.0,0.0],[0.0,0.0,0.0]]
    it "e3" $
      let p = listaMatriz [[3.0,2,3],[1,2,4],[1,2,5]]
      in matrizLista (gauss p)
      `shouldBe` [[3.0,2.0,3.0],[0.0,1.3333333333333335,3.0],[0.0,0.0,1.0]]
    it "e4" $
      let p = listaMatriz [[3%1,2,3],[1,2,4],[1,2,5]]
      in matrizLista (gauss p)
      `shouldBe` [[3 % 1,2 % 1,3 % 1],[0 % 1,4 % 3,3 % 1],[0 % 1,0 % 1,1 % 1]]
    it "e5" $
      let p = listaMatriz [[1.0,0,3],[1,0,4],[3,0,5]]
      in matrizLista (gauss p)
      `shouldBe` [[1.0,3.0,0.0],[0.0,1.0,0.0],[0.0,0.0,0.0]]

  describe "gaussCAux" $
    it "e1" $
      gaussCAux (listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]]) 1 1 0
      `shouldBe` (1,array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),2.0),
                                         ((2,1),0.0),((2,2),1.0),((2,3),0.0),
                                         ((3,1),0.0),((3,2),0.0),((3,3),0.0)])

  describe "gaussC" $
    it "e1" $
      gaussC (listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]])
      `shouldBe` (1,array ((1,1),(3,3)) [((1,1),1.0),((1,2),3.0),((1,3),2.0),
                                         ((2,1),0.0),((2,2),1.0),((2,3),0.0),
                                         ((3,1),0.0),((3,2),0.0),((3,3),0.0)])

  describe "determinante" $
    it "e1" $
      determinante (listaMatriz [[1.0,2,3],[1,3,4],[1,2,5]])
      `shouldBe` 2.0
