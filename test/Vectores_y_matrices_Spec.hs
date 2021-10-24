module Vectores_y_matrices_Spec (main, spec) where

import Vectores_y_matrices
import Data.Array
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "listaVector" $
    it "e1" $
      listaVector [3,2,5] `shouldBe`
        array (1,3) [(1,3),(2,2),(3,5)]

  describe "listaMatriz" $
    it "e1" $
      listaMatriz [[1,3,5],[2,4,7]] `shouldBe`
        array ((1,1),(2,3)) [((1,1),1),((1,2),3),((1,3),5),
                             ((2,1),2),((2,2),4),((2,3),7)]

  describe "numFilas" $
    it "e1" $
      numFilas (listaMatriz [[1,3,5],[2,4,7]])  `shouldBe`  2

  describe "numColumnas" $
    it "e1" $
      numColumnas (listaMatriz [[1,3,5],[2,4,7]])  `shouldBe`  3

  describe "dimension" $
    it "e1" $
      dimension (listaMatriz [[1,3,5],[2,4,7]])  `shouldBe`  (2,3)

  describe "separa" $
    it "e1" $
      separa 3 [1..11]  `shouldBe`  [[1,2,3],[4,5,6],[7,8,9],[10,11]]

  describe "matrizLista" $
    it "e1" $
      matrizLista (listaMatriz [[5,1,0],[3,2,6]]) `shouldBe` [[5,1,0],[3,2,6]]

  describe "vectorLista" $
    it "e1" $
      vectorLista (listaVector [3,2,5]) `shouldBe` [3,2,5]

  describe "sumaMatrices" $
    it "e1" $
      matrizLista (sumaMatrices (listaMatriz [[5,1,0],[3,2,6]])
                                (listaMatriz [[4,6,3],[1,5,2]])) `shouldBe`
        [[9,7,3],[4,7,8]]

  describe "filaMat" $ do
    let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
    it "e1" $
      filaMat 2 p `shouldBe` array (1,3) [(1,3),(2,2),(3,6)]
    it "e2" $
      vectorLista (filaMat 2 p) `shouldBe` [3,2,6]

  describe "columnaMat" $ do
    let p = listaMatriz [[5,1,0],[3,2,6],[4,5,7]]
    it "e1" $
      columnaMat 2 p `shouldBe` array (1,3) [(1,1),(2,2),(3,5)]
    it "e2" $
      vectorLista (columnaMat 2 p) `shouldBe` [1,2,5]

  describe "prodEscalar" $ do
    let v = listaVector [3,1,10]
    it "e1" $
      prodEscalar v v `shouldBe` 110

  describe "prodMatrices" $ do
    let p = listaMatriz [[3,1],[2,4]]
        q = listaMatriz [[7],[5]]
    it "e1" $
      prodMatrices p p `shouldBe`
        array ((1,1),(2,2)) [((1,1),11),((1,2),7),((2,1),14),((2,2),18)]
    it "e2" $
      matrizLista (prodMatrices p p) `shouldBe` [[11,7],[14,18]]
    it "e3" $
      prodMatrices p q `shouldBe`
        array ((1,1),(2,1)) [((1,1),26),((2,1),34)]
    it "e4" $
      matrizLista (prodMatrices p q) `shouldBe` [[26],[34]]

  describe "identidad" $
    it "e1" $
      identidad 3 `shouldBe`
        array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
                             ((2,1),0),((2,2),1),((2,3),0),
                             ((3,1),0),((3,2),0),((3,3),1)]

  describe "potencia" $ do
    let g2 = listArray ((1,1),(2,2)) [1,1,1,0]
    it "e1" $
      potencia g2 2 `shouldBe`
        array ((1,1),(2,2)) [((1,1),2),((1,2),1),((2,1),1),((2,2),1)]
    it "e2" $
      potencia g2 3 `shouldBe`
        array ((1,1),(2,2)) [((1,1),3),((1,2),2),((2,1),2),((2,2),1)]
    it "e3" $
      potencia g2 4 `shouldBe`
        array ((1,1),(2,2)) [((1,1),5),((1,2),3),((2,1),3),((2,2),2)]

  describe "traspuesta" $ do
    let p = listaMatriz [[5,1,0],[3,2,6]]
    it "e1" $
      traspuesta p `shouldBe`
        array ((1,1),(3,2)) [((1,1),5),((1,2),3),
                             ((2,1),1),((2,2),2),
                             ((3,1),0),((3,2),6)]
    it "e2" $
      matrizLista (traspuesta p) `shouldBe`
        [[5,3],[1,2],[0,6]]

  describe "esCuadrada" $ do
    it "e1" $
      esCuadrada (listaMatriz [[5::Int,1,0],[3,2,6]]) `shouldBe` False
    it "e2" $
      esCuadrada (listaMatriz [[5::Int,1],[3,2]]) `shouldBe` True

  describe "esSimetrica" $ do
    it "e1" $
      esSimetrica (listaMatriz [[5,1,3],[1,4,7],[3,7,2]]) `shouldBe`
        True
    it "e2" $
      esSimetrica (listaMatriz [[5,1,3],[1,4,7],[3,4,2]]) `shouldBe`
        False

  describe "diagonalPral" $ do
    it "e1" $
      diagonalPral (listaMatriz [[5,1,0],[3,2,6]]) `shouldBe`
        array (1,2) [(1,5),(2,2)]
    it "e2" $
      vectorLista (diagonalPral (listaMatriz [[5,1,0],[3,2,6]]))
        `shouldBe` [5,2]

  describe "diagonalSec" $ do
    let p = listaMatriz [[5,1,0],[3,2,6]]
        q = traspuesta p
    it "e1" $
      diagonalSec p
        `shouldBe` array (1,2) [(1,1),(2,3)]
    it "e2" $
      vectorLista (diagonalSec p)
        `shouldBe` [1,3]
    it "e3" $
      matrizLista q
        `shouldBe` [[5,3],[1,2],[0,6]]
    it "e4" $
      vectorLista (diagonalSec q)
        `shouldBe` [3,1]

  describe "submatriz" $ do
    let p = listaMatriz [[5,1,0],[3,2,6],[4,6,9]]
    it "e1" $
      submatriz 2 3 p
        `shouldBe` array ((1,1),(2,2)) [((1,1),5),((1,2),1),((2,1),4),((2,2),6)]
    it "e2" $
      matrizLista (submatriz 2 3 p)
        `shouldBe` [[5,1],[4,6]]

  describe "determinante" $ do
    it "e1" $
      determinante (listArray ((1,1),(3,3)) [2,0,0,0,3,0,0,0,1])
        `shouldBe` 6.0
    it "e2" $
      determinante (listArray ((1,1),(3,3)) [1..9])
        `shouldBe` 0.0
    it "e3" $
      determinante (listArray ((1,1),(3,3)) [2,1,5,1,2,3,5,4,2])
        `shouldBe` -33.0
