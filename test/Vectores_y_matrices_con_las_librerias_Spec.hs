{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Vectores_y_matrices_con_las_librerias_Spec (main, spec) where

import Vectores_y_matrices_con_las_librerias
import qualified Data.Vector as V
import Data.Matrix
import Data.Ratio
import Data.Maybe
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "listaVector" $
    it "e1" $
      listaVector [3,2,5]
      `shouldBe` V.fromList [3,2,5]

  describe "listaMatriz" $
    it "e1" $
      listaMatriz [[1,3,5],[2,4,7]]
      `shouldBe` fromLists [[1,3,5],[2,4,7]]

  describe "numFilas" $
    it "e1" $
      numFilas (listaMatriz [[1,3,5],[2,4,7]])  `shouldBe`  2

  describe "numColumnas" $
    it "e1" $
      numColumnas (listaMatriz [[1,3,5],[2,4,7]])  `shouldBe`  3

  describe "dimension" $
    it "e1" $
      dimension (listaMatriz [[1,3,5],[2,4,7]])  `shouldBe`  (2,3)

  describe "dimension" $
    it "e1" $
      let m = listaMatriz [[5,1,0],[3,2,6]]
      in matrizLista m
      `shouldBe` [[5,1,0],[3,2,6]]

  describe "vectorLista" $
    it "e1" $
      let v = listaVector [3,2,5]
      in vectorLista v
      `shouldBe` [3,2,5]

  describe "suma" $
    it "e1" $
      let m1 = listaMatriz [[5,1,0],[3,2,6]]
          m2 = listaMatriz [[4,6,3],[1,5,2]]
      in m1 + m2
      `shouldBe` fromLists [[9,7,3],[4,7,8]]

  describe "filaMat" $ do
    let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
    it "e1" $
      filaMat 2 p `shouldBe` V.fromList [3,2,6]
    it "e2" $
      vectorLista (filaMat 2 p) `shouldBe` [3,2,6]

  describe "columnaMat" $ do
    let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
    it "e1" $
      columnaMat 2 p `shouldBe` V.fromList [1,2,5]
    it "e2" $
      vectorLista (columnaMat 2 p) `shouldBe` [1,2,5]

  describe "prodEscala" $
    it "e1" $
      let v = listaVector [3,1,10]
      in prodEscalar v v
      `shouldBe` 110

  describe "prodMatrices" $ do
    let p = listaMatriz [[3,1],[2,4]]
        q = listaMatriz [[7],[5]]
    it "e1" $
      prodMatrices p p `shouldBe` fromLists [[11,7],[14,18]]
    it "e2" $
      prodMatrices p q `shouldBe` fromLists [[26],[34]]

  describe "traspuesta" $
    it "e1" $
      traspuesta (listaMatriz [[5,1,0],[3,2,6]])
      `shouldBe` fromLists [[5,3],[1,2],[0,6]]

  describe "esCuadrada" $ do
    it "e1" $
      esCuadrada (listaMatriz [[5,1,0],[3,2,6]])
      `shouldBe` False
    it "e2" $
      esCuadrada (listaMatriz [[5,1],[3,2]])
      `shouldBe` True

  describe "esSimetrica" $ do
    it "e1" $
      esSimetrica (listaMatriz [[5,1,3],[1,4,7],[3,7,2]])
      `shouldBe` True
    it "e2" $
      esSimetrica (listaMatriz [[5,1,3],[1,4,7],[3,4,2]])
      `shouldBe` False

  describe "diagonalPral" $
    it "e1" $
      diagonalPral (listaMatriz [[5,1,0],[3,2,6]])
      `shouldBe` V.fromList [5,2]

  describe "diagonalSec" $ do
    let p = listaMatriz [[5,1,0],[3,2,6]]
        q = traspuesta p
    it "e1" $
      diagonalSec p
      `shouldBe` V.fromList [1,3]
    it "e1" $
      diagonalSec q
      `shouldBe` V.fromList [3,1]

  describe "submatriz" $
    it "e1" $
      submatriz 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])
      `shouldBe` fromLists [[5,1],[4,6]]

  describe "intercambiaFilas" $
    it "e1" $
      intercambiaFilas 1 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])
      `shouldBe` fromLists [[4,6,9],[3,2,6],[5,1,0]]

  describe "intercambiaColumnas" $
    it "e1" $
      intercambiaColumnas 1 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])
      `shouldBe` fromLists [[0,1,5],[6,2,3],[9,6,4]]

  describe "multFilaPor" $
    it "e1" $
      multFilaPor 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])
      `shouldBe` fromLists [[5,1,0],[9,6,18],[4,6,9]]

  describe "sumaFilaFila" $
    it "e1" $
      sumaFilaFila 2 3 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])
      `shouldBe` fromLists [[5,1,0],[7,8,15],[4,6,9]]

  describe "sumaFilaPor" $
    it "e1" $
      sumaFilaPor 2 3 10 (listaMatriz [[5,1,0],[3,2,6],[4,6,9]])
      `shouldBe` fromLists [[5,1,0],[43,62,96],[4,6,9]]

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
      let p = listaMatriz [[2,3,1],[5,0,5],[8,6,9]] :: Matrix Double
      in matrizLista (anulaEltoColumnaDesde p 2 1)
      `shouldBe` [[2.0,3.0,1.0],[5.0,0.0,5.0],[4.0,0.0,7.0]]

  describe "anulaColumnaDesde" $ do
    it "e1" $
      let p = listaMatriz [[2,2,1],[5,4,5],[10,8,9]] :: Matrix Double
      in matrizLista (anulaColumnaDesde p 2 1)
      `shouldBe` [[2.0,2.0,1.0],[1.0,0.0,3.0],[2.0,0.0,5.0]]
    it "e2" $
      let p = listaMatriz [[4,5],[2,7%2],[6,10]]
      in matrizLista (anulaColumnaDesde p 1 1)
      `shouldBe` [[4 % 1,5 % 1],[0 % 1,1 % 1],[0 % 1,5 % 2]]

  describe "elementosNoNulosColDesde" $
    it "e1" $
      let p = listaMatriz [[3,2],[5,1],[0,4]]
      in elementosNoNulosColDesde p 1 2
      `shouldBe` [5]

  describe "existeColNoNulaDesde" $ do
    it "e1" $
      let p = listaMatriz [[3,2,5],[5,0,0],[6,0,0]]
      in existeColNoNulaDesde p 2 2
      `shouldBe` False
    it "e1" $
      let q = listaMatriz [[3,2,5],[5,7,0],[6,0,0]]
      in existeColNoNulaDesde q 2 2
      `shouldBe` True

  describe "menorIndiceColNoNulaDesde" $ do
    it "e1" $
      let p = listaMatriz [[3,2,5],[5,7,0],[6,0,0]]
      in menorIndiceColNoNulaDesde p 2 2
      `shouldBe` Just 2
    it "e2" $
      let q = listaMatriz [[3,2,5],[5,0,0],[6,0,2]]
      in menorIndiceColNoNulaDesde q 2 2
      `shouldBe` Just 3
    it "e3" $
      let r = listaMatriz [[3,2,5],[5,0,0],[6,0,0]]
      in menorIndiceColNoNulaDesde r 2 2
      `shouldBe` Nothing

  describe "gaussAux" $
    it "e1" $
      gaussAux (listaMatriz [[1.0,2,3],[1,2,4],[3,2,5]]) 2 2
      `shouldBe` fromLists [[1,2,3],[1,2,4],[2,0,1]]

  describe "gauss" $ do
    it "e1" $
      gauss (listaMatriz [[1.0,2,3],[1,2,4],[1,2,5]])
      `shouldBe` fromLists [[1,3,2],[0,1,0],[0,0,0]]
    it "e2" $
      gauss (listaMatriz [[3%1,2,3],[1,2,4],[1,2,5]])
      `shouldBe` fromLists [[3 % 1, 2 % 1, 3 % 1],
                            [0 % 1, 4 % 3, 3 % 1],
                            [0 % 1, 0 % 1, 1 % 1]]
    it "e3" $
      gauss (listaMatriz [[1.0,0,3],[1,0,4],[3,0,5]])
      `shouldBe` fromLists [[1, 3, 0],
                            [0, 1, 0],
                            [0, 0, 0]]

  describe "gaussCAux" $
    it "e1" $
      gaussCAux (fromLists [[1.0,2,3],[1,2,4],[1,2,5]]) 1 1 0
      `shouldBe` (1,fromLists [[1, 3, 2],
                               [0, 1, 0],
                               [0, 0, 0]])

  describe "gaussC" $
    it "e1" $
      gaussC (fromLists [[1.0,2,3],[1,2,4],[1,2,5]])
      `shouldBe` (1,fromLists [[1, 3, 2],
                               [0, 1, 0],
                               [0, 0, 0]])

  describe "determinante" $
    it "e1" $
      determinante (fromLists [[1.0,2,3],[1,3,4],[1,2,5]])
      `shouldBe` 2.0
